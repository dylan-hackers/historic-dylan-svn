Module:    internals
Synopsis:  HTTP Support
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//define class <sealed-constructor> (<object>) end;
define sealed domain make(subclass(<sealed-constructor>));
define sealed domain initialize(<sealed-constructor>);

define function make-locator (netloc :: false-or(<http-server-url>),
                              dir :: <simple-object-vector>,
                              name :: false-or(<string>),
                              type :: false-or(<string>),
                              query :: false-or(<string>),
                              // note request uri's don't have tags, it's a browser thang.
                              tag :: false-or(<string>))
  let dir = make(<directory-url>, server: netloc, relative?: #f, path: dir);
  if (type | name | tag | query)
    make(<file-url>, directory: dir, base: name, extension: type,
         cgi-string: query, index: tag)
  else
    dir
  end;
end make-locator;

define function decode-url
    (str :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (str :: <string>)
  iterate count (pos :: <integer> = bpos, n :: <integer> = 0)
    let pos = char-position('%', str, pos, epos);
    if (pos)
      if (pos + 3 <= epos)
        count(pos + 3, n + 2)
      else
        invalid-url-encoding-error();
      end;
    elseif (n == 0)
      substring(str, bpos, epos)
    else // Ok, really have to copy...
      let nlen = epos - bpos - n;
      let nstr = make(<byte-string>, size: nlen);
      iterate copy (i :: <integer> = 0, pos :: <integer> = bpos)
        unless (pos == epos)
          let ch = str[pos];
          if (ch ~== '%')
            nstr[i] := ch;
            copy(i + 1, pos + 1);
          else
            let c1 = digit-weight(str[pos + 1]);
            let c2 = digit-weight(str[pos + 2]);
            if (c1 & c2)
              nstr[i] := as(<byte-character>, c1 * 16 + c2);
              copy(i + 1, pos + 3);
            else
              invalid-url-encoding-error();
            end;
          end;
        end unless;
      end iterate;
      nstr
    end if;
  end iterate;
end decode-url;

define function parse-request-uri (str, str-beg, str-end)
  => (uri :: <url>) // <http-url>, but that's bogus.
  parse-uri(str, str-beg, str-end)
    | invalid-uri-error(uri: substring(str, str-beg, str-end));
end;

define function parse-uri (str, str-beg, str-end)
    => (uri :: false-or(<url>))
  // Assumed to be either absolute URI (i.e. "scheme:...") or
  // absolute path (i.e. "/...").  Doesn't accept relative path.
  // For now, only accepts http: as scheme.
  if (str-beg == str-end)
    #f  // This should probably treat "" the same as "/" (according to RFC 2616) --sigue
  elseif (str[str-beg] == '/')
    let (dir, name, type, query, tag) = parse-uri-path(str, str-beg, str-end);
    dir & make-locator(#f, dir, name, type, query, tag);
  elseif (looking-at?("http://", str, str-beg, str-end))
    let net-beg = str-beg + 7;
    let net-end = char-position('/', str, net-beg, str-end) | str-end;
    let netloc = parse-http-server(str, net-beg, net-end);
    let (dir, name, type, query, tag) = if (net-end == str-end)
                                          parse-uri-path("/", 0, 1)
                                        else
                                          parse-uri-path(str, net-end, str-end)
                                        end;
    dir & netloc & make-locator(netloc, dir, name, type, query, tag);
  else
    //---TODO: here should distinguish between an unknown scheme and a relative path.
    #f
  end;
end parse-uri;


define function parse-http-server (str :: <byte-string>,
                                   net-beg :: <integer>,
                                   net-end :: <integer>)
  => (netloc :: false-or(<http-server-url>))
  let host-end = char-position(':', str, net-beg, net-end) | net-end;
  let host = decode-url(str, net-beg, host-end);
  let port = if (host-end == net-end)
               80
             else
               //---TODO: should decode-url this as well, in theory...
               string->integer(str, host-end + 1, net-end)
             end;
  host & port & make(<http-server-url>, host: host, port: port);
end parse-http-server;

 //---TODO: should intern these, i.e. map the whole thing to its parsed version...

// dir is #f if parse failed.
define function parse-uri-path (str, str-beg, str-end)
=> (dir :: false-or(<simple-object-vector>),
    name :: false-or(<string>),
    type :: false-or(<string>),
    query :: false-or(<string>))
  assert(str[str-beg] == '/');
  let path-end = char-position('?', str, str-beg, str-end) | str-end;
  let segs = make(<stretchy-vector>);
  iterate loop (beg = str-beg)
    let beg = beg + 1;
    let pos = char-position('/', str, beg, path-end);
    if (pos)
      let seg = decode-url(str, beg, pos);
      if (seg)
        add!(segs, seg);
        loop(pos);
      else
        values(#f, #f, #f, #f);
      end;
    else
      let segs = as(<simple-object-vector>, segs);
      let dot-pos = char-position-from-end('.', str, beg, path-end);
      let name = decode-url(str, str-beg, dot-pos | path-end);
      let type = dot-pos & decode-url(str, dot-pos + 1, path-end);
      let query = (path-end ~== str-end) & substring(str, path-end + 1, str-end);
      values(segs, name, type, query)
    end;
  end iterate;
end parse-uri-path;

/*
(defun request-search-values (request)
  (let ((old (request-cached-search-values request)))
    (if (eq old $cache-empty)
      (setf (request-cached-search-values request)
            (nconc (with-string-bounds (string start end) (request-query-string request)
                     (parse-search-string string start end))
                   (with-string-bounds (string start end) (request-post-string request)
                     (parse-search-string string start end))))
      old)))

(defun parse-search-string (string start end)
  (let ((values nil))
    (unless (eq start end)
      (loop
        (let ((pos (or (char-position #\+ string start end) end)))
          (push (decode-url string start pos) values)
          (when (eq pos end) (return))
          (setq start (%i+ pos 1)))))
    (nreverse values)))
  

;; ---TODO: add support for fetching something as a vector, e.g. foo[1]...

(defun request-form-value (request key &optional field)
  (let ((val (gethash (request-form-values request) key)))
    (if (null field) ; NIL => Return primary value(s)
      (if (consp val) (car val) val)
      (let ((plist (if (consp val) (cdr val) nil)))
        (if (eq field T)
          plist
          (getf plist field))))))

(defun request-form-values (request)
  (or (request-cached-form-values request)
      (setf (request-cached-form-values request)
            (let ((table (request-form-values-table request)))
              (clrhash table)
              (with-string-bounds (string start end) (request-query-string request)
                (when (char-position #\= string start end)
                  (parse-form-string table string start end)))
              (with-string-bounds (string start end) (request-post-string request)
                (parse-form-string table string start end))
              table))))


(defvar *known-form-keys* ())
(defvar *known-form-subkeys* '(:x :y))

(defun parse-form-string (table string start end)
  (unless (eq start end)
    (loop
      (let* ((apos (or (char-position #\& string start end) end))
             (epos (or (char-position #\= string start apos)
                       (error 'invalid-form-encoding)))
             (vpos (%i+ epos 1))
             (mainkey nil)
             (subkey nil)
             (key (multiple-value-bind (kstr kstart kend) (decode-url string start epos t)
                    (let* ((key (intern-key kstr kstart kend *known-form-subkeys*))
                           (vend (char-position #\. kstr kstart kend))
                           (spos (and vend (%i+ vend 1))))
                      (when vend
                        (setq mainkey (intern-key kstr kstart vend *known-form-keys*))
                        (setq subkey (or (%string-to-integer kstr spos kend)
                                         (intern-key kstr spos kend *known-form-subkeys*))))
                      key)))
             ;; TODO: should we replace +'s in value with spaces?
             (value (unless (eq vpos apos) (decode-url string vpos apos)))
             (old (gethash key table nil)))
        (cond ((not old) (setf (gethash key table) value))
              ((stringp old) (setf (gethash key table) (cons (list value old) nil)))
              ((stringp (car old)) (setf (car old) (list value (car old))))
              (t (setf (car old) (cons value (car old)))))
        (when mainkey
          (let ((old (gethash mainkey table nil)))
            (when (not old)
              (setf (gethash mainkey table) (setq old (cons nil nil))))
            (let ((oldv (getf (cdr old) subkey)))
              (setf (getf (cdr old) subkey)
                    (cond ((null oldv) value)
                          ((stringp oldv) (list value oldv))
                          (t (cons value oldv)))))))
        (when (eq apos end) (return))
        (setq start (%i+ apos 1))))))


(defun decode-url (string start end &optional segment-ok)
  ;; returns (new-string new-start new-end)
  ;; If segment-ok is nil, always returns a complete string, i.e. new-start=0, new-end=length(new-string).
  ;; If segment-ok is :replace, just decodes in place, i.e. new-string = string.
  (let ((num (count #\% string :start start :end end)))
    (if (and segment-ok (eq num 0))
      (values string start end)
      (let* ((olen (%i- end start))
             (nlen (%i- olen (%i+ num num))))
        (multiple-value-bind (nstring nstart)
                             (if (eq segment-ok :replace)
                               (values string start)
                               (values (make-string nlen :element-type 'base-character) 0))
          (do ((outpos nstart (%i+ outpos 1)))
              ((eq start end)
               (values nstring nstart outpos))
            (let ((ch (%schar string start)))
              (setq start (%i+ start 1))
              (if (eq ch #\%)
                (let ((cend (%i+ start 2)))
                  (setf (%scharcode nstring outpos)
                        (or (and (%i<= cend end) (%hex-to-integer string start cend))
                            (error 'invalid-url-escaping)))
                  (setq start cend))
asdfadsfj
                (setf (%schar nstring outpos) ch)))))))))

*/

