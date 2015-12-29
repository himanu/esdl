module esdl.intf.fli;

import std.traits: isIntegral;
import esdl.data.bvec;

struct SignalIdTag;
struct DriverIdTag;
struct TypeIdTag;
struct RegionIdTag;
struct VariableIdTag;
struct ProcessIdTag;

alias SignalIdT = SignalIdTag*;
alias DriverIdT = DriverIdTag*;
alias TypeIdT   = TypeIdTag*;
alias RegionIdT = RegionIdTag*;
alias VariableIdT = VariableIdTag*;
alias ProcessIdT = ProcessIdTag*;

enum TypeKindT {
  _SCALAR   =  0,        /* Integer types                          */
  _ARRAY    =  1,
  _RECORD   =  2,
  _ENUM     =  3,
  _INTEGER  =  4,        /* not used (use MTI_TYPE_SCALAR instead) */
  _PHYSICAL =  5,
  _REAL     =  6,
  _ACCESS   =  7,
  _FILE     =  8,
  _TIME     =  9,
  _REG      = 10,
  _NET      = 11,
  _MEMELEM  = 13,
  _C_REAL   = 15,
  _VL_ENUM  = 19,
  _WREAL    = 46,
  _C_ENUM   = 264
}

enum DirectionT {
  INTERNAL,
  DIR_IN,
  DIR_OUT,
  DIR_INOUT
}

union GenericValUnion {
  int             generic_value;         /* Integer/physical/enum generic     */
  /* value                             */
  double          generic_value_real;    /* Real generic value                */
  long            generic_value_time;    /* Time generic value                */
  void*           generic_array_value;   /* Array generic value               */
  VariableIdT     generic_record_varid;  /* Generic record variable           */
  SignalIdT       port;                  /* Signal ID of port                 */
  version(IBMCC) {
    long          _qalign_natural;        /** force 8-byte alignment of union **/
  }
}
    

struct InterfaceListStruct {
  char*          name;                  /* Simple name of generic/port       */
  TypeIdT        type;                  /* Tnype of generic/port              */
  DirectionT     port_dir;              /* Direction of port                 */
                                             /* (All generics are INTERNAL)       */
  GenericValUnion u;
  InterfaceListT* nxt;                  /* Next generic/port in list         */
};

alias InterfaceListT = InterfaceListStruct;

enum ProcessTriggerT {
    ACTIVE,
    EVENT
}

struct instanceInfoT {
  SignalIdT sigid;
  DriverIdT drvid;
  TypeIdT time_type;
}

enum DriverModeEnum {
  INERTIAL,
  TRANSPORT
}

// typedef void (*mtiVoidFuncPtrT)        PROTO((void * param));

alias VoidFuncPtrT = void function(void *);

extern(C)
void mti_Sensitize (ProcessIdT proc, SignalIdT sig, ProcessTriggerT when);

void fliSensitize(ProcessIdT proc, SignalIdT sig, ProcessTriggerT when) {
  mti_Sensitize(proc, sig, when);
}

extern(C)
ProcessIdT  mti_CreateProcess (char * name, VoidFuncPtrT func, void * param);
ProcessIdT  fliCreateProcess (string name,
			      VoidFuncPtrT func,
			      void * param) {
  import std.string;
  return mti_CreateProcess(cast(char*) name.toStringz(), func, param);
}

extern(C) void* mti_Malloc (ulong size);
void* fliMalloc (ulong size) {
  return mti_Malloc(size);
}

extern(C) SignalIdT mti_FindPort(InterfaceListT * list, char * name);

SignalIdT fliFindPort(InterfaceListT * list, string name) {
  import std.string;
  return mti_FindPort(list, cast(char*) name.toStringz());
}

extern(C)
void mti_ScheduleDriver (DriverIdT drv, long value, int delay, DriverModeEnum mod);

void fliScheduleDriver (DriverIdT drv, long value, int delay, DriverModeEnum mod) {
  mti_ScheduleDriver(drv, value, delay, mod);
}

extern(C)
void mti_ScheduleDriver64 (DriverIdT drv, long value, long delay, DriverModeEnum mode);
void fliScheduleDriver64 (DriverIdT drv, long value, long delay, DriverModeEnum mode) {
  mti_ScheduleDriver64(drv, value, delay, mode);
}

extern(C) int mti_GetSignalValue (SignalIdT sig);

int fliGetSignalValue (SignalIdT sig) {
  return mti_GetSignalValue(sig);
}

extern(C) void mti_SetSignalValue (SignalIdT sig, size_t val);
void fliSetSignalValue (SignalIdT sig, size_t val) {
  mti_SetSignalValue(sig, val);
}

extern(C) DriverIdT   mti_CreateDriver(SignalIdT sig);
DriverIdT fliCreateDriver(SignalIdT sig) {
  return mti_CreateDriver(sig);
}

extern(C) TypeKindT mti_GetTypeKind (TypeIdT type);
TypeKindT fliGetTypeKind (TypeIdT type) {
  return mti_GetTypeKind(type);
}


extern(C) TypeIdT mti_GetSignalType(SignalIdT sig);
TypeIdT fliGetSignalType(SignalIdT sig) {
  return mti_GetSignalType(sig);
}

extern(C) int mti_TickLength(TypeIdT tp);
int fliTickLength(TypeIdT tp) {
  return mti_TickLength(tp);
}

extern(C) void* mti_GetSignalValueIndirect(SignalIdT sig,
					   void * buf);
void* fliGetSignalValueIndirect(SignalIdT sig, void* buf) {
  return mti_GetSignalValueIndirect(sig, buf);
}
				
extern(C) SignalIdT* mti_GetSignalSubelements (SignalIdT sig,
						SignalIdT * buf);
SignalIdT* fliGetSignalSubelements (SignalIdT sig,
				    SignalIdT * buf) {
  return mti_GetSignalSubelements(sig, buf);
}



enum StdLogicEnum {
  _U,
  _X,
  _0,
  _1,
  _Z,
  _W,
  _L,
  _H,
  _D
}

void setStdLogicSignalValue(T)(SignalIdT sig, T t)
  if(isIntegral!T || isBitVector!T || is(T == bool)) {
    static if(isIntegral!T) {
      enum SIZE = T.sizeof * 8;
    }
    static if(isBitVector!T) {
      enum SIZE = T.SIZE;
    }
    static if(is(T == bool)) {
      enum SIZE = 1;
    }
    
    static if(SIZE == 1) {
      // assert(fliGetTypeKind(fliGetSignalType(sig)) == TypeKindT._SCALAR,
      // 	     "Signal size does not match");
    }
    else {
      assert(SIZE == fliTickLength(fliGetSignalType(sig)),
    	     "Signal size does not match");
    }

    static if(SIZE == 1) {
      static if(isBitVector!T) {
	if(t.isSame(LOGIC_0)) {
	  fliSetSignalValue(sig, StdLogicEnum._0);
	}
	if(t.isSame(LOGIC_1)) {
	  fliSetSignalValue(sig, StdLogicEnum._1);
	}
	if(t.isSame(LOGIC_X)) {
	  fliSetSignalValue(sig, StdLogicEnum._X);
	}
	if(t.isSame(LOGIC_Z)) {
	  fliSetSignalValue(sig, StdLogicEnum._Z);
	}
      }
      else {			// bool
	if(t is true) {
	  fliSetSignalValue(sig, StdLogicEnum.STD_LOGIC_1);
	}
	else {
	  fliSetSignalValue(sig, StdLogicEnum.STD_LOGIC_0);
	}
      }
    }
    else {
      static if(isBitVector!T) {
	alias bits = t;
      }
      else {
	Logic!SIZE bits = t;
      }
      
      SignalIdT[SIZE] elems;
      fliGetSignalSubelements(sig, elems.ptr);

      for (size_t i=0; i!=SIZE; ++i) {
      	if(bits[i] == BIT_0 || bits[i] == LOGIC_0) {
      	  fliSetSignalValue(elems[SIZE-i-1], StdLogicEnum._0);
      	}
	if(bits[i] == BIT_1 || bits[i] == LOGIC_1) {
	  fliSetSignalValue(elems[SIZE-i-1], StdLogicEnum._1);
	}
      	if(bits[i].isSame(LOGIC_X)) {
      	  fliSetSignalValue(elems[SIZE-i-1], StdLogicEnum._X);
      	}
      	if(bits[i].isSame(LOGIC_Z)) {
      	  fliSetSignalValue(elems[SIZE-i-1], StdLogicEnum._Z);
      	}
      }
    }
  }

void getStdLogicSignalValue(T)(SignalIdT sig, ref T t)
  if(isIntegral!T || isBitVector!T || is(T == bool)) {
    static if(isIntegral!T) {
      enum SIZE = T.sizeof * 8;
    }
    static if(isBitVector!T) {
      enum SIZE = T.SIZE;
    }
    static if(is(T == bool)) {
      enum SIZE = 1;
    }
    
    static if(SIZE == 1) {
      // assert(fliGetTypeKind(fliGetSignalType(sig)) == TypeKindT._SCALAR,
      // 	     "Signal size does not match");
    }
    else {
      assert(SIZE == fliTickLength(fliGetSignalType(sig)),
    	     "Signal size does not match");
    }

    static if(SIZE == 1) {
      static if(isBitVector!T) {
	StdLogicEnum val = cast(StdLogicEnum) fliGetSignalValue(sig);
	final switch(val) {
	case StdLogicEnum._U:
	case StdLogicEnum._X:
	case StdLogicEnum._D:
	case StdLogicEnum._W:
	  t = LOGIC_X;
	  break;
	case StdLogicEnum._0:
	case StdLogicEnum._L:
	  t = LOGIC_0;
	  break;
	case StdLogicEnum._1:
	case StdLogicEnum._H:
	  t = LOGIC_1;
	  break;
	case StdLogicEnum._Z:
	  t = LOGIC_Z;
	  break;
	}
      }
      else {			// bool
	int val = fliGetSignalValue(sig);
	if(val == StdLogicEnum._0 || val == StdLogicEnum._L) {
	  t = false;
	}
	else {
	  t = true;
	}
      }
    }
    else {
      static if(isBitVector!T) {
	alias bits = t;
      }
      else {
	Bits[SIZE] bits;
      }
      
      SignalIdT[SIZE] elems;
      fliGetSignalSubelements(sig, elems.ptr);

      for (size_t i=0; i!=SIZE; ++i) {
	StdLogicEnum val = cast(StdLogicEnum) fliGetSignalValue(elems[SIZE-i-1]);
	final switch(cast(StdLogicEnum) val) {
	case StdLogicEnum._U:
	case StdLogicEnum._X:
	case StdLogicEnum._D:
	case StdLogicEnum._W:
	  bits[i] = LOGIC_X;
	  break;
	case StdLogicEnum._0:
	case StdLogicEnum._L:
	  bits[i] = LOGIC_0;
	  break;
	case StdLogicEnum._1:
	case StdLogicEnum._H:
	  bits[i] = LOGIC_1;
	  break;
	case StdLogicEnum._Z:
	  bits[i] = LOGIC_Z;
	  break;
	}
      }

      static if(isIntegral!T) {
	t = bits;
      }
      
      
    }
  }
  
