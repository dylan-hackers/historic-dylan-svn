#ifndef DYLANDEFD
#define DYLANDEFD
#include "dylan-debugger.h"

#define DYLAN_POINTER_TYPE (TYPE_POINTER_TYPE(builtin_type_void))
#define DYLAN_POINTER_SIZE (TYPE_LENGTH(TYPE_POINTER_TYPE(builtin_type_void)))

#define DYLAN_MM_WRAPPER_I_IMPLEMENTATION_CLASS 0

#define DYLAN_IMPLEMENTATION_CLASS_I_CLASS 0
#define DYLAN_IMPLEMENTATION_CLASS_I_MM_WRAPPER 1
#define DYLAN_IMPLEMENTATION_CLASS_I_SUBTYPE_MASK 2
#define DYLAN_IMPLEMENTATION_CLASS_I_SUBTYPE_BIT 3
#define DYLAN_IMPLEMENTATION_CLASS_I_REPEATED_SLOT_DESCRIPTOR 4
#define DYLAN_IMPLEMENTATION_CLASS_I_INSTANCE_SLOT_DESCRIPTORS 5
#define DYLAN_IMPLEMENTATION_CLASS_I_DISPATCH_KEY 6

#define DYLAN_CLASS_I_INSTANCEP_IEP 0
#define DYLAN_CLASS_I_DEBUG_NAME 1
#define DYLAN_CLASS_I_IMPLEMENTATION_CLASS 2
#define DYLAN_CLASS_I_MM_WRAPPER 3
#define DYLAN_CLASS_I_MANGLED_NAME 4


#define MAX_NAME_SIZE 1000

extern char* copy_string(char*);

#include "dylan-accessors.h"
#include "dylan-mangler.h"
#include "dylan-print.h"

#endif
