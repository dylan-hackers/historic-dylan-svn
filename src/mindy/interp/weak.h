/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  All rights reserved.
*  
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*  
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports, questions, comments, and suggestions should be sent by
*  E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
*
***********************************************************************
*
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/weak.h,v 1.4 1996/02/02 01:52:32 wlott Exp $
*
\**********************************************************************/


struct weak_pointer {
    obj_t class;
    obj_t object;
    boolean broken;
    struct weak_pointer *next;
};

#define WEAK(o) obj_ptr(struct weak_pointer *, o)

extern obj_t obj_WeakPointerClass;
extern obj_t make_weak_pointer(obj_t object);

extern void weak_pointer_gc_setup(void);
extern void break_weak_pointers(void);
