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


template <typename T>
struct desc_const_iterator // : std::random_access_iterator<T, std::ptrdiff_t>
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
  std::vector<const Type*> params(extr(argtypes->begin()), extr(argtypes->end()));
  return FunctionType::get(result, params, isVarArg);
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

/*
BinaryOperator* make_llvm_Add(Value* v1, Value* v2, const ByteString* name, BasicBlock* atEnd, Instruction* before)
{
  return make_llvm_BinaryOperator(Instruction::Add, v1, v2, name, atEnd, before);
}
*/

typedef BinaryOperator
  BinaryAdd,
  BinarySub,
  BinaryMul,
  BinaryDiv,
  BinaryRem,
  BinaryAnd,
  BinaryOr,
  BinaryXor,
  BinarySetEQ,
  BinarySetNE,
  BinarySetLE,
  BinarySetGE,
  BinarySetLT,
  BinarySetGT;

#define LLVM_MAKE_BINARY(OP) \
  LLVM_MAKE(Binary ## OP, (Value* v1, Value* v2, const ByteString* name, BasicBlock* atEnd, Instruction* before)) \
  { return make_llvm_BinaryOperator(Instruction::OP, v1, v2, name, atEnd, before); }

LLVM_MAKE_BINARY(Add)
LLVM_MAKE_BINARY(Sub)
LLVM_MAKE_BINARY(Mul)
LLVM_MAKE_BINARY(Div)
LLVM_MAKE_BINARY(Rem)
LLVM_MAKE_BINARY(And)
LLVM_MAKE_BINARY(Or)
LLVM_MAKE_BINARY(Xor)
LLVM_MAKE_BINARY(SetEQ)
LLVM_MAKE_BINARY(SetNE)
LLVM_MAKE_BINARY(SetLE)
LLVM_MAKE_BINARY(SetGE)
LLVM_MAKE_BINARY(SetLT)
LLVM_MAKE_BINARY(SetGT)


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




