Module:    genetics-editor
Synopsis:  Creatures 2 genetics editor
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

/// "File" command table

define command-table *file-command-table* (*global-command-table*)
  menu-item "New..."     = new-document,
    documentation: "Make a new document";
  menu-item "Open..."    = open-document,
    documentation: "Open an existing document";
  menu-item "Close"      = close-document,
    documentation: "Close the current document";
  separator;
  menu-item "Load GNO File..."      = apply-gno-file,
    documentation: "Applies the selected GNO file descriptions to the current genome.";
  separator;
  menu-item "Save"       = save-document,
    documentation: "Save the current document";
  menu-item "Save As..." = save-document-as,
    documentation: "Save to a new document";
  separator;
  menu-item "Exit"       = exit-frame,
    documentation: "Exit the application";
end command-table *file-command-table*;


/// "Edit" command table
/*
define command-table *edit-command-table* (*global-command-table*)
//  menu-item "Undo"  = make(<command>, function: command-undo),
//    documentation: "Undo the last change";
//  menu-item "Redo"  = make(<command>, function: command-redo),
//    documentation: "Redo the last undone change";
//  separator;
//  menu-item "Cut"   = make(<command>, function: clipboard-cut),
//    documentation: "Cut to the clipboard";
//  menu-item "Copy"  = make(<command>, function: clipboard-copy),
//    documentation: "Copy to the clipboard";
//  menu-item "Paste" = make(<command>, function: clipboard-paste),
//    documentation: "Paste from the clipboard";
end command-table *edit-command-table*;
*/

/// "Help" command table

define command-table *help-command-table* (*global-command-table*)
  menu-item "Topics..." = <help-on-topics>,
    documentation: "Show documentation for a topic";
  separator;
  menu-item "About..."  = <help-on-version>,
    documentation: "Show the About box for this application";
end command-table *help-command-table*;


/// Application command table

define command-table *template-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
//  menu-item "Edit" = *edit-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *template-command-table*;


/// Application frame

//*** FILL THE REST OF THIS IN ***
define class <genetics-editor-document> (<object>)
  slot document-pathname = #f,
    init-keyword: pathname:;
  slot document-gno-pathname = #f;

  slot document-modified? :: <boolean> = #f,
    init-keyword: modified?:;

  // Each entry is a <genome-item>;
  slot document-genome = #f, init-keyword: genome:;
end class <genetics-editor-document>;

define method get-gene-short-description( genome-item )
    if( genome-item.item-gno ) 
      genome-item.item-gno.gno-description;
    else
      genome-item.item-gene.gene-short-description;
    end if;      
end method get-gene-short-description;

define method set-gene-short-description( genome-item, description )
    if( genome-item.item-gno)
      genome-item.item-gno.gno-description := description;
    else
      let gene = genome-item.item-gene;
      let entry = make(<gno-documentation>,
                       type: gene.gene-type,
                       subtype: gene.gene-subtype,
                       number: gene.gene-sequence-number,
                       description: description,
                       comment: "");
      genome-item.item-gno := entry;
    end if;      
end method set-gene-short-description;


//*** FILL THE REST OF THIS IN ***
define frame <genetics-editor-frame> (<simple-frame>)
  slot frame-current-sort = 0;

  pane gene-list-pane (frame)  
    make(<table-control>,      
      enabled?: #f,
      headings: #("No.", "Description", "Type", "Sequence", "Duplicate"),
      widths: #(30, 180, 100, 30, 30),
      generators: vector( item-index, 
                          get-gene-short-description,
                          compose(gene-type-name, item-gene),
                          compose(gene-sequence-number, item-gene),
                          compose(gene-duplicate-number, item-gene)),
      callbacks: vector(number-sort, description-sort, gene-type-sort, gene-type-sort, gene-type-sort),
      popup-menu-callback: on-popup-menu,
      value-changed-callback: on-gene-list-activate);

  pane no-gene-displayed-pane (frame)
    make(<text-field>, text: "No gene selected.", enabled?: #f);

  pane add-button (frame)
    make(<push-button>, label: "Add", activate-callback: on-add-button);

  pane remove-button (frame)
    make(<push-button>, label: "Remove", activate-callback: on-remove-button);

  pane move-button (frame)
    make(<push-button>, label: "Move", activate-callback: on-move-button);

  pane gene-display-pane ( frame )
    make(<tab-control>,
        pages: vector(make(<tab-control-page>,
                            label: "No Gene", child: frame.no-gene-displayed-pane)));  
  layout (frame)
    make(<row-splitter>, 
      ratios: #(310, 330), 
      children: vector(make(<tab-control>,
                            pages: vector(make(<tab-control-page>,
                                               label: "Gene List",
                                               child: vertically()
                                                        frame.gene-list-pane;
                                                        horizontally()
                                                          frame.add-button;
                                                          frame.remove-button;
                                                          frame.move-button;
                                                        end;
                                                      end))),
                                          frame.gene-display-pane));
                       
  command-table (frame)
    *template-command-table*;
  status-bar (frame)
    make(<status-bar>);
  keyword title: = $application-name;
  keyword width: = 640;
  keyword height: = 400;
end frame <genetics-editor-frame>;

define method handle-event( frame :: <genetics-editor-frame>, event :: <frame-mapped-event> ) => ()
  let hwnd = frame.gene-list-pane.window-handle;
  let LVS-EX-FULLROWSELECT = #x20;
  let LVM-FIRST = #x1000;
  let LVM-GETEXTENDEDLISTVIEWSTYLE = LVM-FIRST + #x37;
  let LVM-SETEXTENDEDLISTVIEWSTYLE = LVM-FIRST + #x36;
  let lstyle = SendMessage(hwnd, LVM-GETEXTENDEDLISTVIEWSTYLE, 0, 0);
  lstyle := %logior(lstyle, LVS-EX-FULLROWSELECT);
  SendMessage(hwnd, LVM-SETEXTENDEDLISTVIEWSTYLE, 0, lstyle);
end method handle-event;  

/// File open commands
define variable *status-postfix* :: <string> = ".";

define method frame-document-setter
    (document :: false-or(<genetics-editor-document>), frame :: <genetics-editor-frame>)
 => (document :: false-or(<genetics-editor-document>))
  if(document)
    frame.frame-current-sort := 0;
    frame.gene-list-pane.gadget-items := *sorts*[0](document.document-genome);
    frame.frame-status-bar.gadget-label := format-to-string("%d genes loaded%s", size(document.document-genome), *status-postfix*);
    *status-postfix* := ".";
    frame.frame-title := concatenate(if(document.document-pathname)
        document.document-pathname
      else
        "Untitled"
      end if, " - ", $application-name);
  end if;
  gadget-enabled?(frame.gene-list-pane) := (document & #t);
  next-method()
end method frame-document-setter;

define method make-indexer()
    let count = 0;
    method( gene )
      let result = make(<genome-item>, index: count, gene: gene);
      count := count + 1;
      result;
    end method;      
end method make-indexer;
define method new-document
    (frame :: <genetics-editor-frame>) => (document :: <genetics-editor-document>)

    frame.frame-document := make(<genetics-editor-document>, 
      modified?: #t,
      genome: map( make-indexer(), make-genome() ));  
end method new-document;

define constant $genome-file-filters = #[
    #["Genome Files (*.gen)", "*.gen"],
    #["All Files (*.*)", "*.*"]
];

define constant $gno-file-filters = #[
    #["Genome Description Files (*.gno)", "*.gno"],
    #["All Files (*.*)", "*.*"]
];


define method create-gno-name( filename )
    // Quick and dirty...
    concatenate( copy-sequence( filename, start: 0, end: filename.size - 3 ), "gno" );
end method create-gno-name;

define method open-document
    (frame :: <genetics-editor-frame>, #key document) => (document :: false-or(<genetics-editor-document>))
 let file = choose-file(direction: #"input", owner: frame, filters: $genome-file-filters);
 if( file )
   block()
     with-busy-cursor( frame )
       // Load genome.
       let (genome, success?) = load-genome( file );
       when(~success?)
         notify-user("Warning: Could not load complete genome.");
       end when;

       // Try to find the gno file
       let gno-filename = create-gno-name(file);
       let gno-documentation = #f;
       if(file-exists?( gno-filename ) )
         block()
           gno-documentation := load-gno-file( gno-filename, version: genome[0].gene-genome-version);
         exception(e :: <condition>)
           notify-user("There was an error loading the GNO file.");
         end block;

         *status-postfix* := format-to-string(", using gno file %s.", gno-filename);
       else
         // No gno file, try a species gno file
         let species-gene = choose(method(x) type-for-copy(x) = <genus-gene> end, genome);
         if(~empty?(species-gene))
           let species-gno = select(species-gene[0].gene-genus-species)
             #"norn" => "norn.gno";
             #"grendel" => "gren.gno";
             #"ettin" => "ettn.gno";
             #"shee" => "shee.gno";
             otherwise => #f;
           end select;
           if(species-gno & file-exists?( species-gno ))     
             block()
               gno-documentation := load-gno-file( species-gno, version: genome[0].gene-genome-version );
               *status-postfix* := format-to-string(", gno file %s not found - using default %s.", gno-filename, species-gno);
             exception(e :: <condition>)
               notify-user("There was an error loading the GNO file.");
             end block;
           end if;
         end if;
       end if;
       if(*status-postfix* = ".")
         *status-postfix* := ", no .gno file found.";
       end if;
                     
       let gno-documentation-normal = 
         if(gno-documentation) 
           choose(rcurry(instance?, <gno-documentation>), gno-documentation);
         else
           #f
         end;
       let gno-documentation-extra = 
         if(gno-documentation) 
           choose(rcurry(instance?, <gno-extra-documentation>), gno-documentation);
         else
           #f
         end;

       let count = 0;
       local method create-index( gene )
         let gno-element = #f;
         let gno-extra-element = #f;
         if(gno-documentation-normal)
           gno-element := find-element( gno-documentation-normal,
                                        method(gd)
                                          gd.gno-type = gene.gene-type &
                                          gd.gno-subtype = gene.gene-subtype &
                                          gd.gno-number = gene.gene-sequence-number;
                                        end);
         end if;
         if(gno-documentation-extra)
           gno-extra-element := choose(method(gd)
                                         gd.gno-type = gene.gene-type &
                                         gd.gno-subtype = gene.gene-subtype &
                                         gd.gno-sequence = gene.gene-sequence-number;
                                       end,
                                       gno-documentation-extra);
           when(zero?(gno-extra-element.size))
             gno-extra-element := #f;
           end;
         end;
         let result = make(<genome-item>, index: count, gene: gene, gno: gno-element,
                           extra-gno: gno-extra-element);
         count := count + 1;
         result;
       end method create-index;

       frame.frame-document := make(<genetics-editor-document>, 
         pathname: file, 
         genome: map( create-index, genome ));  
     end with-busy-cursor;
   exception( condition :: <end-of-stream-error> )
     notify-user("There was an error loading the genome or gno file.");
     frame.frame-document := #f;
   end block;
 end if;
end method open-document;

define method apply-gno-file
    (frame :: <genetics-editor-frame>, #key document) => ()
let document = document | frame.frame-document;   
if(document)
 let file = choose-file(direction: #"input", owner: frame, filters: $gno-file-filters);
 if( file )
   block()
     with-busy-cursor( frame )
         let gno-documentation = #f;
         let genome = document.document-genome;

         block()
           gno-documentation := load-gno-file( file, version: genome[0].item-gene.gene-genome-version);
         exception(e :: <condition>)
           notify-user("There was an error loading the GNO file.");
         end block;

         let gno-documentation-normal = 
           if(gno-documentation) 
             choose(rcurry(instance?, <gno-documentation>), gno-documentation);
           else
             #f
           end;
         let gno-documentation-extra = 
           if(gno-documentation) 
             choose(rcurry(instance?, <gno-extra-documentation>), gno-documentation);
           else
            #f
           end;


         for(gene-item in frame.frame-document.document-genome)
           let gene = gene-item.item-gene;
           let gno-element = #f;
           let gno-extra-element = #f;

           if(gno-documentation-normal)
             gno-element := find-element( gno-documentation-normal,
                                          method(gd)
                                            gd.gno-type = gene.gene-type &
                                            gd.gno-subtype = gene.gene-subtype &
                                            gd.gno-number = gene.gene-sequence-number;
                                          end);
           end if;
           if(gno-documentation-extra)
             gno-extra-element := choose(method(gd)
                                          gd.gno-type = gene.gene-type &
                                          gd.gno-subtype = gene.gene-subtype &
                                          gd.gno-sequence = gene.gene-sequence-number;
                                         end,
                                         gno-documentation-extra);
             when(zero?(gno-extra-element.size))
               gno-extra-element := #f;
             end;
           end;

           gene-item.item-gno := gno-element;
           gene-item.item-extra-gno := gno-extra-element;
         end for;
         let save = frame.gene-list-pane.gadget-items;
         frame.gene-list-pane.gadget-items := #[];
         frame.gene-list-pane.gadget-items := save;
     end with-busy-cursor;
   exception( condition :: <end-of-stream-error> )
     notify-user("There was an error loading the gno file.");
   end block;
 end if;
end if;
end method apply-gno-file;


define method close-document
    (frame :: <genetics-editor-frame>, #key document) => ()
  let document = frame-document(frame);
  //*** FILL THE REST OF THIS IN ***
  frame-document(frame) := #f;
end method close-document;


define method save-genome-and-gno( document )
  let gno-filename = create-gno-name(document.document-pathname);

  let gno-vector = make(<stretchy-vector>);
  for(item in document.document-genome)
    when(item.item-gno)
      gno-vector := add!(gno-vector, item.item-gno);
    end;
    when(item.item-extra-gno)
      for(extra in item.item-extra-gno)
        gno-vector := add!(gno-vector, extra);
      end for;
    end when;
  end for;

  if(~empty?(gno-vector))
         save-gno-file(gno-vector , gno-filename);
  end;
  save-genome( map(item-gene, document.document-genome), document.document-pathname );
end method save-genome-and-gno;

/// File save commands

define method save-document
    (frame :: <genetics-editor-frame>, #key document) => ()
  let document = document | frame-document(frame);
  if(document & document.document-pathname)
    with-busy-cursor( frame )
      save-genome-and-gno( document );
    end with-busy-cursor;
  elseif(document & ~document.document-pathname)
    save-document-as(frame, document: document);
  else
    notify-user("You must create a new document or open an existing one first",
		owner: frame,
		style: #"information", exit-style: #"ok");
  end
end method save-document;

define method save-document-as
    (frame :: <genetics-editor-frame>, #key document) => ()
  let document = document | frame-document(frame);
  if (document)
    let file = choose-file(direction: #"output", 
      owner: frame, 
      default: document.document-pathname,
      filters: $genome-file-filters);
    if (file)
      if(~find-element( file, curry(\=, '.' ) ) )
        file := concatenate( file, ".gen" );
      end if;
      with-busy-cursor( frame )
        document-pathname(document) := file;
        save-genome-and-gno( document );
        frame.frame-title := concatenate(document.document-pathname, " - ", $application-name);
      end with-busy-cursor;
    end
  else
    notify-user("You must create a new document or open an existing one first",
		owner: frame,
		style: #"information", exit-style: #"ok");
  end
end method save-document-as;

define method note-document-changed
    (frame :: <genetics-editor-frame>, document :: <genetics-editor-document>) => ()
  document-modified?(document) := #t;
  //*** FILL THE REST OF THIS IN ***
end method note-document-changed;

/// Undo/redo commands

define method command-undo
    (frame :: <genetics-editor-frame>) => ()
  //*** FILL THE REST OF THIS IN ***
end method command-undo;

define method command-redo
    (frame :: <genetics-editor-frame>) => ()
  //*** FILL THE REST OF THIS IN ***
end method command-redo;


/// Clipboard commands
/*
define method clipboard-cut
    (frame :: <genetics-editor-frame>) => ()
  let gadget = frame.main-pane;	// *** GADGET TO CUT FROM ***
  with-clipboard (clipboard = gadget)
    when (clipboard)
      add-clipboard-data(clipboard, gadget-value(gadget))
    end
  end
  //*** NOW REMOVE THE DATA FROM THE GADGET ***
end method clipboard-cut;

define method clipboard-copy
    (frame :: <genetics-editor-frame>) => ()
  let gadget = frame.main-pane;	// *** GADGET TO COPY FROM ***
  with-clipboard (clipboard = gadget)
    when (clipboard)
      add-clipboard-data(clipboard, gadget-value(gadget))
    end
  end
end method clipboard-copy;

define method clipboard-paste
    (frame :: <genetics-editor-frame>) => ()
  let gadget = frame.main-pane;	// *** GADGET TO PASTE INTO ***
  with-clipboard (clipboard = gadget)
    when (clipboard)
      let text = get-clipboard-data-as(<string>, clipboard);
      when (text)
	gadget-value(gadget) := text
      end
    end
  end
end method clipboard-paste;
*/

/// Help commands
define method do-execute-command
    (frame :: <genetics-editor-frame>, command :: <help-on-topics>) => ()
  //*** REMOVE THIS IF YOU ADD A HELP SOURCE ***
  notify-user("No help available.", owner: frame)
end method do-execute-command;

define method do-execute-command
    (frame :: <genetics-editor-frame>, command :: <help-on-version>) => ()
  let text = concatenate(application-full-name(),
                         "\nCopyright (c) 1999, Chris Double.",
                         "\nAll Rights Reserved.\n",
                         "Using creatures-genes.dll V", creatures-genes-version(), "\n",
                         "Using ui-creatures-genes.dll V", ui-creatures-genes-version(), "\n",
                         "Using gno-file.dll V", gno-file-version(), "\n",
                         "Using double-rich-text-gadget.dll V", rich-text-gadget-version(), "\n",
                         "http://www.double.co.nz/creatures");
  notify-user(text, owner: frame)
end method do-execute-command;

define method on-gene-list-activate(gadget, #key blank = #f) 
  let frame = gadget.sheet-frame;
  let tab-control = frame.gene-display-pane;
  if(blank)
    tab-control.tab-control-pages :=  vector(make(<tab-control-page>,
                            label: "No Gene", child: frame.no-gene-displayed-pane));  
  else
    let genome-item :: <genome-item> = gadget.gadget-value;
    let gene-pane = make-pane-for-gene(genome-item.item-gene, genome: gadget.gadget-items);
    let has-comment? = genome-item.item-gno & genome-item.item-gno.gno-comment.size > 0;
    let comment-label = if(has-comment?) "Comments (*)" else "Comments" end if;
    let comment-page = make(<tab-control-page>,
                            label: comment-label,
                            child: vertically() 
                                    make(<rich-text-gadget>,
                                         text: if(has-comment?)
                                                 genome-item.item-gno.gno-comment
                                               else
                                                 ""
                                               end if) end);
    let has-extra-comment? = genome-item.item-extra-gno;
    let pages = if(has-extra-comment?)
                  let extra-text = "";
                  for(extra in genome-item.item-extra-gno)
                    for(line from 0 below extra.gno-comments.size)
                      when(extra.gno-comments[line].size > 0)
                        extra-text := concatenate(extra-text, format-to-string("Line %d: ", 
                                                                               line + 1),
                                                  extra.gno-comments[line],
                                                  "\n");
                      end when;
                    end for;
                  end for;
                  let extra-page = make(<tab-control-page>,
                                        label: "C3 Comments",
                                        child: vertically()
                                                 make(<rich-text-gadget>,
                                                      read-only?: #t,
                                                      text: extra-text);
                                               end);
                   vector(make(<tab-control-page>, label: genome-item.get-gene-short-description,
                              child: gene-pane), comment-page, extra-page);
                                                 
                else
                  vector(make(<tab-control-page>, label: genome-item.get-gene-short-description,
                              child: gene-pane), comment-page);
                end if;

    tab-control.tab-control-pages := pages;
  end if;
end method on-gene-list-activate;

define method type-sort( lhs, rhs, predicate )
    let lhs-gene = lhs.item-gene;
    let rhs-gene = rhs.item-gene;

    if(predicate( lhs-gene.gene-type-name, rhs-gene.gene-type-name ) )
      #t
    elseif(lhs-gene.gene-type-name = rhs-gene.gene-type-name &
           lhs-gene.gene-sequence-number < rhs-gene.gene-sequence-number )
      #t
    elseif(lhs-gene.gene-type-name = rhs-gene.gene-type-name &
           lhs-gene.gene-sequence-number = rhs-gene.gene-sequence-number &
           lhs-gene.gene-duplicate-number < rhs-gene.gene-duplicate-number )
      #t
    else
      #f;
    end if; 
   
end method type-sort;

define function index-comparison( lhs :: <genome-item>, rhs :: <genome-item>, predicate :: <function> )
  if(predicate(lhs.item-index, rhs.item-index))
    #t
  else
    #f
  end if;
end function index-comparison;
define variable *sorts* = vector(
    rcurry(sort, test:, rcurry(index-comparison, \<)),
    rcurry(sort, test:, rcurry(index-comparison, \>)),
    method(items) sort(items, test: method(lhs, rhs) lhs.get-gene-short-description < rhs.get-gene-short-description end) end,
    method(items) sort(items, test: method(lhs, rhs) lhs.get-gene-short-description > rhs.get-gene-short-description end) end,
    rcurry(sort, test: rcurry( type-sort, \< ) ),
    rcurry(sort, test: rcurry( type-sort, \> ) )
);

define method number-sort(g)
    let frame = sheet-frame(g);
  with-busy-cursor( frame )
    frame.frame-current-sort := if(frame.frame-current-sort = 0) 1 else 0 end;
    let items = frame.gene-list-pane.gadget-items;
    frame.gene-list-pane.gadget-items := #[];
    frame.gene-list-pane.gadget-items := *sorts*[frame.frame-current-sort](items);
  end;
end method number-sort;

define method description-sort(g)
    let frame = sheet-frame(g);
  with-busy-cursor( frame )
    frame.frame-current-sort := if(frame.frame-current-sort = 2) 3 else 2 end;
    frame.gene-list-pane.gadget-items := *sorts*[frame.frame-current-sort](frame.gene-list-pane.gadget-items);
  end;
end method description-sort;

define method gene-type-sort(g)
    let frame = sheet-frame(g);
  with-busy-cursor( frame )
    frame.frame-current-sort := if(frame.frame-current-sort = 4) 5 else 4 end;
    frame.gene-list-pane.gadget-items := *sorts*[frame.frame-current-sort](frame.gene-list-pane.gadget-items);
  end;
end method gene-type-sort;


define method on-add-button(g)
  let frame = g.sheet-frame;
  let document = frame.frame-document;
  if(document)
    let (class, ok?) = choose-from-dialog($gene-classes, 
      title: "Pick a gene",
      label-key: gene-class-description,
      value-key: identity);
    if (ok?)
      let genome = document.document-genome;
      let version = 
        if(genome.size == 0)
          #"creatures2"
        else
          genome[0].item-gene.gene-genome-version
        end if;
        
      block()
        let gene = create-gene(version, class.gene-class-type, class.gene-class-subtype);
        let genome = document.document-genome;

        let last-gene = last(genome);
        let last-number = last-gene.item-index + 1;
    
        with-busy-cursor( frame )
          let item = make(<genome-item>, index: last-number, gene: gene);
          add!( genome, item );
          add-item ( frame.gene-list-pane, make-item( frame.gene-list-pane, item ) );
          frame.frame-status-bar.gadget-label := 
            format-to-string("%s genes loaded.", size(genome));
          frame.gene-list-pane.gadget-value := item;
          on-gene-list-activate(frame.gene-list-pane);
        end;
      exception( e :: <condition> )
        notify-user("Could not create the gene for version of Creatures the current genome is for.");
      end block;
    end if;
  end if;
end method on-add-button;

define method on-remove-button(g)
  // Delete the currently selected item in the list box.
  let frame = sheet-frame(g);
  let document = frame.frame-document;
  if(document & frame.gene-list-pane.gadget-value)    
    let listbox = frame.gene-list-pane;
    let gene = listbox.gadget-value;
    with-busy-cursor( frame )
      let item-found? = find-item( listbox, gene );
      when(item-found?)      
        remove-item ( listbox, find-item( listbox, gene ) );
      end when;
      remove!( document.document-genome, gene );
      unless(item-found?)
        listbox.gadget-value := #f;
        listbox.gadget-items := #[];
        listbox.gadget-items := document.document-genome;
      end unless;
      on-gene-list-activate(listbox, blank: #t);
      frame.frame-status-bar.gadget-label := format-to-string("%s genes loaded.", size(document.document-genome));
    end;
  end if;
end method on-remove-button;

define method on-move-button(g)
  let frame = g.sheet-frame;
  let document = frame.frame-document;
  if(document  & frame.gene-list-pane.gadget-value)
    let genome = document.document-genome;
    let listbox = frame.gene-list-pane;

    let (before, ok?) = choose-from-dialog(
      genome,
      title: "Move before what gene?",
      label-key: method(x) format-to-string("%d - %s", x.item-index, gene-short-description(x.item-gene)) end,
      value-key: identity);
    if (ok?)
      let current-gene = listbox.gadget-value;
      let new-genome = make(<stretchy-vector>);
      with-busy-cursor( frame )
        for(gene in genome)
          if(gene == before)
            add!( new-genome, current-gene );
          end if;

          if(gene ~= current-gene)
            add!( new-genome, gene );
          end if;
        end for;

        let count = 0;
        local method create-index( gene )
          let result = make(<genome-item>, index: count, gene: gene.item-gene, gno: gene.item-gno);
          count := count + 1;
          result;
        end method create-index;
  
        frame.frame-document := make(<genetics-editor-document>, 
          pathname: frame.frame-document.document-pathname, 
          modified?: #t,
          genome: map( create-index, new-genome ));  

        listbox.gadget-value := first(choose(method(x) x.item-gene == current-gene.item-gene end, 
                                       frame.frame-document.document-genome));

        on-gene-list-activate(listbox);
      end;
    end if;
  end if;
end method on-move-button;

define method on-popup-menu(table, target, #key x, y)
   when(target)
    let menu = make(<menu>,
                    owner: table.sheet-frame,
                    children: vector(
                      make(<push-menu-button>, label: "Rename", activate-callback: method(g) on-rename-gene(table.sheet-frame, target) end)
                      ));
    display-menu(menu);
   end when;
end method on-popup-menu;

define frame <rename-gene-frame> (<simple-frame>)
    slot frame-element, required-init-keyword: element:;

    pane text-pane (frame)
      make(<text-field>, text: get-gene-short-description(frame.frame-element));

    layout (frame)
      vertically()
        frame.text-pane;
        horizontally()
          make(<push-button>, label: "OK", activate-callback: 
            method(g) 
              set-gene-short-description(frame.frame-element, frame.text-pane.gadget-value);
              exit-frame(g.sheet-frame);
            end);
          make(<push-button>, label: "Cancel", activate-callback: method(g) exit-frame(g.sheet-frame) end);
        end;
      end;
end frame <rename-gene-frame>;

define method on-rename-gene( frame, gene )
    start-frame(make(<rename-gene-frame>, owner: frame, mode: #"modal", element: gene, title: "Rename Gene")); 
    with-busy-cursor( frame )
      update-gadget(frame.gene-list-pane);
    end;
end method on-rename-gene;

/// Start the template

define method start-template
    () => ()
  start-frame(make(<genetics-editor-frame>));
end method start-template;
