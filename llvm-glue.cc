#define __STDC_LIMIT_MACROS

#include <llvm/DerivedTypes.h>
#include <llvm/BasicBlock.h>
#include <llvm/Module.h>
#include <llvm/Instructions.h>

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

extern "C" BasicBlock* make_llvm_BasicBlock(const ByteString* name, Function* function, BasicBlock* before)
{
  return new BasicBlock(*name, function, before);
}

extern "C" FunctionType* make_llvm_FunctionType(const Type* result, const SimpleObjectVector* argtypes, bool isVarArg)
{
  std::vector<const Type*> params/*()*/;
  return FunctionType::get(result, params, isVarArg);
}

extern "C" Module* make_llvm_Module(const ByteString* name)
{
  return new Module(*name);
}

extern "C" Function* make_llvm_Function(FunctionType* type, const ByteString* name, Module* module)
{
  return new Function(type, GlobalValue::ExternalLinkage, *name, module);
}

extern "C" ReturnInst* make_llvm_ReturnInst(/*val, beforeinstr*/ BasicBlock* atEnd)
{
  return new ReturnInst(atEnd);
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

#define LLVM_TYPEID2TYPE(ID) \
extern "C" const Type* get_llvm_ ## ID(void) \
{ \
  return Type::getPrimitiveType(Type::ID); \
}

LLVM_TYPEID2TYPE(BoolTyID)
LLVM_TYPEID2TYPE(UByteTyID)
LLVM_TYPEID2TYPE(SByteTyID)
LLVM_TYPEID2TYPE(UShortTyID)
LLVM_TYPEID2TYPE(ShortTyID)
LLVM_TYPEID2TYPE(UIntTyID)
LLVM_TYPEID2TYPE(IntTyID)
LLVM_TYPEID2TYPE(ULongTyID)
LLVM_TYPEID2TYPE(LongTyID)
LLVM_TYPEID2TYPE(FloatTyID)
LLVM_TYPEID2TYPE(DoubleTyID)
LLVM_TYPEID2TYPE(LabelTyID)




