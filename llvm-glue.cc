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
    typedef const ELEM* const_iterator;

    std::size_t size(void) const { return elems; }
    iterator begin(void) { return arr; }
    iterator end(void) { return arr + elems; }
    const_iterator begin(void) const { return arr; }
    const_iterator end(void) const { return arr + elems; }
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


#define LLVM_MAKE(CLASS, ARGLIST) \
  extern "C" CLASS* make_llvm_ ## CLASS ARGLIST

#define LLVM_MAKE_SIMPLE(CLASS, ARGLIST, HOW) \
  LLVM_MAKE(CLASS, ARGLIST) { return new CLASS HOW; }

LLVM_MAKE_SIMPLE(BasicBlock, (const ByteString* name, Function* function, BasicBlock* before), (*name, function, before))
LLVM_MAKE_SIMPLE(Module, (const ByteString* name), (*name))
LLVM_MAKE_SIMPLE(Function, (FunctionType* type, const ByteString* name, Module* module), (type, GlobalValue::ExternalLinkage, *name, module))
LLVM_MAKE_SIMPLE(ReturnInst, (/*val, beforeinstr*/ BasicBlock* atEnd), (atEnd))


// desc_const_iterator
template <typename T>
struct desc_const_iterator
{
  desc_const_iterator(const desc* d)
  : d(d)
  {}
  
  T operator * (void) const { return extract(*d, static_cast<T*>(0)); }
  std::ptrdiff_t operator - (const desc_const_iterator<T>& rhs) const { return d - rhs.d; }
  desc_const_iterator<T>& operator ++ (void) { ++d; return *this; }

private:
  static inline long extract(const desc& d, long*)
  { return d.dataword.l; }
  
  static inline T extract(const desc& d, T*)
  { return static_cast<T>(d.dataword.ptr); }
  
  const desc* d;
};

// iterator_traits
namespace std {
  template <typename T>
  struct iterator_traits< desc_const_iterator<T> >
  {
    typedef typename iterator_traits<T*>::iterator_category iterator_category;
    typedef T value_type;
    typedef ptrdiff_t difference_type;
  };
}


LLVM_MAKE(FunctionType, (const Type* result, const SimpleObjectVector* argtypes, bool isVarArg))
{
  typedef desc_const_iterator<const Type*> extr;
  const std::vector<const Type*> params(extr(argtypes->begin()), extr(argtypes->end()));
  return FunctionType::get(result, params, isVarArg);
}

LLVM_MAKE(PointerType, (const Type* base))
{
  return PointerType::get(base);
}

LLVM_MAKE(StructType, (const SimpleObjectVector* membertypes))
{
  typedef desc_const_iterator<const Type*> extr;
  const std::vector<const Type*> members(extr(membertypes->begin()), extr(membertypes->end()));
  return StructType::get(members);
}

LLVM_MAKE(GetElementPtrInst, (Value* ptr, const SimpleObjectVector* indices, const ByteString* name, BasicBlock* atEnd, Instruction* before))
{
  typedef desc_const_iterator<Value*> extr;
  const extr first(indices->begin());
  if (2 == indices->size())
  {
    extr second(first);
    ++second;
    return before
      ? new GetElementPtrInst(ptr, *first, *second, *name, before)
      : new GetElementPtrInst(ptr, *first, *second, *name, atEnd);
  }
  else
  {
    const std::vector<Value*> inds(first, extr(indices->end()));
    return before
      ? new GetElementPtrInst(ptr, inds, *name, before)
      : new GetElementPtrInst(ptr, inds, *name, atEnd);
  }
}


LLVM_MAKE(StoreInst, (Value* val, Value* ptr, BasicBlock* atEnd, Instruction* before))
{
  return before
    ? new StoreInst(val, ptr, before)
    : new StoreInst(val, ptr, atEnd);
}

LLVM_MAKE(LoadInst, (Value* ptr, const ByteString* name, BasicBlock* atEnd, Instruction* before))
{
  return before
    ? new LoadInst(ptr, *name, before)
    : new LoadInst(ptr, *name, atEnd);
}

LLVM_MAKE_SIMPLE(Argument, (const Type* ty, const ByteString* name, Function* fun), (ty, *name, fun))
/*
LLVM_MAKE_SIMPLE(, , )
LLVM_MAKE_SIMPLE(, , )
*/

LLVM_MAKE(BinaryOperator, (Instruction::BinaryOps op, Value* v1, Value* v2, const ByteString* name, BasicBlock* atEnd, Instruction* before))
{
  return before
	 ? BinaryOperator::create(op, v1, v2, *name, before)
	 : BinaryOperator::create(op, v1, v2, *name, atEnd);
}

#define LLVM_MAKE_BINARY(OP) \
  typedef BinaryOperator Binary ## OP; \
  LLVM_MAKE(Binary ## OP, (Value* v1, Value* v2, const ByteString* name, BasicBlock* atEnd, Instruction* before)) \
  { return make_llvm_BinaryOperator(Instruction::OP, v1, v2, name, atEnd, before); }


// generate a LLVM_MAKE_BINARY for each binary instruction
#define HANDLE_BINARY_INST(num, opcode, Class) \
  LLVM_MAKE_BINARY(opcode)
#include <llvm/Instruction.def>
#undef HANDLE_BINARY_INST

/*
// Memory operators...
 FIRST_MEMORY_INST(21)
HANDLE_MEMORY_INST(21, Malloc, MallocInst)  // Heap management instructions
HANDLE_MEMORY_INST(22, Free  , FreeInst  )
HANDLE_MEMORY_INST(23, Alloca, AllocaInst)  // Stack management
HANDLE_MEMORY_INST(24, Load  , LoadInst  )  // Memory manipulation instrs
HANDLE_MEMORY_INST(25, Store , StoreInst )
HANDLE_MEMORY_INST(26, GetElementPtr, GetElementPtrInst)
*/


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



#define LLVM_TYPEID2TYPE(ID) \
  extern "C" const Type* get_llvm_ ## ID(void) \
  { \
    return Type::getPrimitiveType(Type::ID); \
  }

LLVM_TYPEID2TYPE(VoidTyID)
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




// iterator stuff

extern "C" void* get_llvm_Iterator_Function_arg_begin(Function* f)
{
  return f->arg_begin();
}