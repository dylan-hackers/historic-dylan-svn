#define __STDC_LIMIT_MACROS

#include <llvm/DerivedTypes.h>
#include <llvm/BasicBlock.h>
#include <llvm/Module.h>

namespace d2c
{
  struct Obj
  {
    struct Class* c;
  };
  
  template <typename PREFIX, typename ELEM>
  struct ArrayObject : PREFIX
  {
    std::size_t elems;
    ELEM arr[];

    typedef ELEM* iterator;
    iterator begin(void) { return arr; }
    iterator end(void) { return arr + elems; }
  };

  struct ByteString : ArrayObject<Obj, unsigned char>
  {
    operator std::string (void) const
    {
      const char* start(reinterpret_cast<const char*>(arr));
      return std::string(start, elems);
    }
  };

  struct Symbol : Obj
  {
    ByteString* print_name;
  };

  struct Class : Obj
  {
    Symbol* name;
  };
  
  struct desc
  {
    Obj* heapptr;
    union {
        long l;
        float f;
        void *ptr;
    } dataword;
  };


  struct SimpleObjectVector : ArrayObject<Obj, desc>
  {
  };



}

using namespace llvm;
using namespace d2c;

extern "C" void* make_llvm_BasicBlock(void/*NAME, FUNC, BEFORE*/)
{
  return new BasicBlock();
}

extern "C" void* make_llvm_FunctionType(const Type* result, const SimpleObjectVector* argtypes, bool isVarArg)
{
  std::vector<const Type*> params/*()*/;
  return FunctionType::get(result, params, isVarArg);
}

extern "C" void* make_llvm_Module(const ByteString* name)
{
  return new Module(*name);
}

extern "C" void* make_llvm_Function(FunctionType* type, const ByteString* name, Module* module)
{
  return new Function(type, GlobalValue::ExternalLinkage, *name, module);
}


#define LLVM_DELETER(CLASS) \
extern "C" void delete_llvm_ ## CLASS(CLASS* o) \
{ \
  delete o; \
}

#define LLVM_NULLARY(CLASS, METHOD) \
extern "C" void dump_llvm_ ## CLASS(CLASS* o) \
{ \
  o->METHOD(); \
}

LLVM_DELETER(Module)
LLVM_NULLARY(Module, dump)

LLVM_NULLARY(Type, dump)

LLVM_DELETER(Value)
LLVM_NULLARY(Value, dump)



extern "C" const Type* get_llvm_VoidTyID(void)
{
  return Type::getPrimitiveType(Type::VoidTyID);
}
