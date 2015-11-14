/*
 * mhpi_user.h
 *
 * The MHDL procedural interface draft standard header file.
 *
 */

//#ifndef MHPI_USER_H
//#define MHPI_USER_H

//#ifdef __cplusplus
//extern "C" {
//#endif

module esdl.intf.mhpi;

version(COSIM_VHDL) {
  extern(C) {
    /***************************** time structure *****************************/
    struct mhpiTimeT{
      uint high;
      uint low;          /* for vpiSimTime */
      double realv;          //Doubt
    };

    /* MHPI Scalar Types */
    alias mhpiBitT = uint;
    alias mhpiCharT = ubyte;
    alias mhpiIntT = int;
    alias mhpiRealT = double;

    /* MHPI object kinds */
    enum mhpiObjectKindT {
      mhpiBlockStmtK,
      mhpiCallbackK,
      mhpiConstDeclK,
      mhpiDriverK,
      mhpiLoadK,
      mhpiEventDeclK,
      mhpiFuncCallK,
      mhpiIteratorK,
      mhpiInstanceK,
      mhpiModuleK,
      mhpiProcessK,
      mhpiEqProcessStmtK,
      mhpiForceK,
      mhpiAssignStmtK,
      mhpiProcDeclK,
      mhpiFuncDeclK,
      mhpiClockingBlockK,
      mhpiScopeK,
      mhpiSigDeclK,
      mhpiVarDeclK,
      mhpiSigParamDeclK,
      mhpiVarParamDeclK,
      mhpiConstParamDeclK,
      mhpiPortDeclK,
      mhpiProcCallStmtK,
      mhpiRootInstK,
      mhpiTopInstK,
      mhpiCompDeclK,
      mhpiPrimitiveK,
      mhpiNamedEventK,
      mhpiPackInstK,
      mhpiPrimTermK,
      mhpiSubtypeDeclK,
      mhpiTypeDeclK,
      mhpiTypeHandleK,
      mhpiIfGenStmtK,
      mhpiForGenStmtK,
      mhpiOperationK,
      mhpiWriterExprK,
      mhpiReaderExprK,
      mhpiIntLiteralK,
      mhpiLiteralK,
      mhpiInterfaceInstK,
      mhpiModPortDeclK,
      mhpiModPortPortDeclK,
      mhpiInterfaceInstPortK,
      mhpiModPortInstPortK,
      mhpiModuleInstK,
      mhpiWaitStmtK,
      mhpiIfStmtK,
      mhpiIfElseStmtK,
      mhpiGenerateStmtK,
      mhpiVirtualSigK,
      mhpiDerefObjK,
      /* Verilog timing check kinds */
      mhpiTchkSetupK,
      mhpiTchkHoldK,
      mhpiTchkSetupHoldK,
      mhpiTchkRecoveryK,
      mhpiTchkRemovalK,
      mhpiTchkRecremK,
      mhpiTchkSkewK,
      mhpiTchkTimeSkewK,
      mhpiTchkFullSkewK,
      mhpiTchkPeriodK,
      mhpiTchkWidthK,
      mhpiTchkNoChangeK,
      mhpiTchkTermK,
      mhpiRefObjK,
      mhpiReleaseK,
      mhpiCaseStmtK,
      mhpiCaseItemStmtK,
      mhpiDeassignStmtK,
      mhpiDelayControlStmtK,
      mhpiDisableStmtK,
      mhpiForLoopStmtK,
      mpiForeverStmtK,
      mhpiParamAssignStmtK,
      mhpiWhileLoopStmtK,
      mhpiRepeatStmtK,
      mhpiClockingIODeclK,
      mhpiEventControlK,
      mhpiIntRangeK,
      mhpiAssertionK,
      mhpiFrameK,
      mhpiUnknownObjK /* has to be always last */
    };


    enum mhpiTypeKindT{
      mhpiTypeBitK,
      mhpiTypeLogicK,
      mhpiTypeBooleanK,
      mhpiTypeIntK,
      mhpiTypeRealK,
      mhpiTypeTimeK,
      mhpiTypeEnumK,
      mhpiTypeArrayK,
      mhpiTypeRecordK,
      mhpiTypeStringK,
      mhpiTypeUserK,
      mhpiTypeCharK,
      mhpiTypeCharLiteralK,
      mhpiTypeFileDeclK,
      mhpiTypeAccessDeclK,
      mhpiTypeStrLiteralK,
      mhpiTypeIntLiteralK,
      mhpiTypeClassK,
      mhpiTypeVirtualSigK,
      mhpiTypeMpPortK,
      mhpiTypePhysK,
      mhpiTypeWireRealK,
      mhpiTypeInvalidK
    };

    /* MHPI handle Type */
    alias mhpiHandleT = ulong*;

    enum mhpiPliT{
      mhpiVhpiPli,
      mhpiAccPli,
      mhpiVpiPli,
      mhpiOvaPli,
      mhpiSyscPli,
      mhpiPpiPli,

      /* please make sure this is the last one */
      mhpiNulPli
    };

    enum mhpiHdlT {
      mhpiVHDL,
      mhpiVerilog,
      mhpiMHDL
    };

    /* MHPI assertion types */
    enum mhpiAssertionTypeT {
      mhpiUnknownAssertionType,
      mhpiOvaForbidAssertion,
      mhpiConcurrentAssertion,
      mhpiImmediateAssertion,
      mhpiImmediateCover,
      mhpiCoverProperty,
      mhpiSequence
    };

    /* MHPI Integer or Boolean Properties */
    enum mhpiIntPropertyT {
      mhpiIsCompositeP,
      mhpiIsMixedP,
      mhpiIsResolvedP,
      mhpiIsScopeP,
      mhpiIsScalarP,
      mhpiLeftBoundP,
      mhpiNumDimensionsP,
      mhpiNumFieldsP,
      mhpiObjectKindP,
      mhpiRightBoundP,
      mhpiTypeKindP,
      mhpiUserBaseTypeKindP,
      mhpiHdlP,
      mhpiModeP,
      mhpiReasonP,
      mhpiSizeP,
      mhpiTimePrecisionP,
      mhpiTimeUnitP,
      mhpiIsVerilogTopP,
      mhpiPliP,
      mhpiDefLineNoP,  /* location of object definition */
      mhpiLineNoP,     /* location of object usage */
      mhpiMacroIndexP,     /* macro index of object usage if it's in a macro*/
      mhpiMacroOffsetP,     /* macro offset of object usage if it's in a macro*/
      mhpiProtectOffsetP, /* protected block offset of object usage if it is in a vip protected block */
      mhpiEndMacroIndexP,     /* macro index of object usage if it's in a macro*/
      mhpiEndMacroOffsetP,     /* macro offset of object usage if it's in a macro*/
      mhpiIsComplexExprP,
      mhpiIsDirectInstP,
      mhpiNameKindP,
      mhpiIsBoundaryInstP,
      mhpiPortKindP,
      mhpiInstanceKindP,
      mhpiIsAssertion,
      mhpiIsGetPutValValidP, /* returns 1 if mhpi_get/put_value can be called
			      * for this object, otherwise returns 0 */
      mhpiIsOvaUnitP,
      mhpiAssertionTypeP,
      mhpiIsValidForUniqFlow,

      mhpiStartLineNoP,
      mhpiEndLineNoP,
      mhpiIsRangeUpP,
      mhpiIsPliDriverP,
      mhpiIsLowPowerDriverP,
      mhpiAceIsVpdMergeP,
      mhpiCanBeDumpedP,   /* Return 1 if the given object can be dumped. */

      mhpiDebugAccessP,   /* Returns the value of the -debug_acc flag. */

      /* new properties should only be added above this one here */
      mhpiZDikeIntP
    };

    /* MHPI String Properties */
    enum mhpiStrPropertyT {
      mhpiFullNameP,
      mhpiKindStrP,
      mhpiNameP,
      mhpiModuleNameP, /* for instance handle*/
      mhpiFullCaseNameP,
      mhpiCaseNameP,
      mhpiObjectKindStrP,
      mhpiTypeKindStrP,
      mhpiDefFileP, /* location of the object definition */
      mhpiFileP,    /* location of the object usage */
      mhpiPortKindStrP,
      mhpiInstanceKindStrP,
      mhpiLocalNameP, /* defined for local name for driver only*/
      mhpiNameWithDimP,
      mhpiFullNameWithDimP,
      mhpiDataTypeStrP,
      mhpiPortDirStrP,
      mhpiDefFullFileP, /* location of the object definition in full path */
      mhpiFullFileP,        /* location of the object usage in full path*/
      mhpiSourceNameP,
      mhpiLPKindStrP, /* Low Power kind */
      mhpiZDikeStrP
    };

    /* MHPI 1 to 1 relationships */
    enum mhpiOneToOneT {
      mhpiElemSubtype, /* for array*/
      mhpiScope,
      mhpiParent,
      mhpiLhs,
      mhpiRhs,
      mhpiModule,
      mhpiExpr,
      mhpiPrimitive,
      mhpiLowConn,
      mhpiHighConn,
      mhpiFormalPort,
      mhpiLocalPort,
      mhpiSimpleName,
      mhpiProcess,
      mhpiDecl,
      /* Verilog timing check relations */
      mhpiTchkRef,
      mhpiTchkTermExpr,
      mhpiTchkTermCond,
      mhpiTchkData,
      mhpiTchkDelayedRef,
      mhpiTchkDelayedData,
      mhpiTchkNotify,
      mhpiActual,
      mhpiCondition,
      mhpiCurScope,
      mhpiZDike /* has to be last */
    };

    /* MHPI 1 to many relationships */
    enum mhpiOneToManyT {
      mhpiBlocks,
      mhpiInternalScopes,
      mhpiRanges, mhpiConstraints = mhpiRanges,
      mhpiDecls,
      mhpiPortDecls,
      mhpiParamDecls,
      mhpiPrimitives,
      mhpiTopInsts,
      mhpiSigDecls,
      mhpiVarDecls,
      mhpiDrivers,
      mhpiLoads,
      mhpiVlLoads,  /*Full loads information for pure verilog design*/
      mhpiFanouts,
      mhpiIndexedNames,
      mhpiSelectedNames,
      mhpiBasicSignals,
      mhpiContributors,
      mhpiPortInsts,
      mhpiProcesses,
      mhpiModuleInsts,
      mhpiPortAssocs,
      mhpiPrimTerms,
      mhpiWriterExprs,
      mhpiReaderExprs,
      mhpiExprLeaves,
      mhpiLocalLoads,
      mhpiFullLoads,
      mhpiLocalBasicLoads,
      mhpiFullBasicLoads,
      mhpiLocalDrivers,
      mhpiFullDrivers,
      mhpiLocalBasicDrivers,
      mhpiFullBasicDrivers,
      mhpiInterfaceInsts,
      mhpiInstances,
      mhpiModPortDecls,
      mhpiGenerateStmts,
      mhpiVirtualSigs,
      mhpiBasicDrivers,
      mhpiBasicLoads,
      mhpiConstDecls,
      mhpiGenericDecls,
      mhpiPortCrossings,
      mhpiTchkStmts,
      mhpiAssertions,
      mhpiSequenceEvents,
      /* mhpiZDikes must be the last enumerator--add above. */
      mhpiZDikes
    };

    /* time units */
    enum mhpi1FS = -15;
    enum mhpi10FS = -14;
    enum mhpi100FS = -13;
    enum mhpi1PS = -12;
    enum mhpi10PS = -11;
    enum mhpi100PS = -10;
    enum mhpi1NS = -9;
    enum mhpi10NS = -8;
    enum mhpi100NS = -7;
    enum mhpi1US = -6;
    enum mhpi10US = -5;
    enum mhpi100US =-4;
    enum mhpi1MS = -3;
    enum mhpi10MS = -2;
    enum mhpi100MS = -1;
    enum mhpi1S = 0;
    enum mhpi10S = 1;
    enum mhpi100S = 2;

    /* MHPI Value Formats */
    enum mhpiValueFormatT {
      mhpiLogicVal,
      mhpi4LogicVecVal,
      mhpi9LogicVecVal,
      mhpiBitVal,
      mhpiBitVecVal,
      mhpiIntVal,
      mhpiIntVecVal,
      mhpiRealVal,
      mhpiRealVecVal,
      mhpiStrVal,
      mhpiBinStrVal,
      mhpiOctStrVal,
      mhpiDecStrVal,
      mhpiHexStrVal,
      mhpiTimeVal,
      mhpiObjTypeVal,
      mhpiStrengthVal,
      mhpiStrengthStrVal,
      mhpiInvalidFormat

    };

    /* MHPI strength values */
    enum mhpiStrengthT {
      mhpiSt0,
      mhpiSt1,
      mhpiHiZ,
      mhpiStX,
      mhpiStL,
      mhpiStH,
      mhpiSu0,
      mhpi760,
      mhpi750,
      mhpi740,
      mhpi730,
      mhpi720,
      mhpi710,
      mhpiSuL,
      mhpi71X,
      mhpi72X,
      mhpi73X,
      mhpi74X,
      mhpi75X,
      mhpi76X,
      mhpiSuX,
      mhpi650,
      mhpi640,
      mhpi630,
      mhpi620,
      mhpi610,
      mhpi61X,
      mhpi62X,
      mhpi63X,
      mhpi64X,
      mhpi65X,
      mhpi67X,
      mhpiPu0,
      mhpi540,
      mhpi530,
      mhpi520,
      mhpi510,
      mhpiPuL,
      mhpi51X,
      mhpi52X,
      mhpi53X,
      mhpi54X,
      mhpiPuX,
      mhpi56X,
      mhpi57X,
      mhpiLa0,
      mhpi430,
      mhpi420,
      mhpi410,
      mhpiLaL,
      mhpi41X,
      mhpi42X,
      mhpi43X,
      mhpiLaX,
      mhpi45X,
      mhpi46X,
      mhpi47X,
      mhpiWe0,
      mhpi320,
      mhpi310,
      mhpiWeL,
      mhpi31X,
      mhpi32X,
      mhpiWeX,
      mhpi34X,
      mhpi35X,
      mhpi36X,
      mhpi37X,
      mhpiMe0,
      mhpi210,
      mhpiMeL,
      mhpi21X,
      mhpiMeX,
      mhpi23X,
      mhpi24X,
      mhpi25X,
      mhpi26X,
      mhpi27X,
      mhpiSm0,
      mhpiSmL,
      mhpiSmX,
      mhpi12X,
      mhpi13X,
      mhpi14X,
      mhpi15X,
      mhpi16X,
      mhpi17X,
      mhpiSmH,
      mhpiMeH,
      mhpiWeH,
      mhpiLaH,
      mhpiPuH,
      mhpiSuH,
      mhpiSm1,
      mhpi211,
      mhpi311,
      mhpi411,
      mhpi511,
      mhpi611,
      mhpi711,
      mhpiMe1,
      mhpi321,
      mhpi421,
      mhpi521,
      mhpi621,
      mhpi721,
      mhpiWe1,
      mhpi431,
      mhpi531,
      mhpi631,
      mhpi731,
      mhpiLa1,
      mhpi541,
      mhpi641,
      mhpi741,
      mhpiPu1,
      mhpi651,
      mhpi751,
      mhpi761,
      mhpiSu1
    };

    /* MHPI Value Data Structure. Almost replica of VPI Value Data Structure
       without strength fields*/
    /* vector value */
    struct mhpi4LogicVecValT {
      /* following fields are repeated enough times to contain vector */
      mhpiIntT aVal;
      mhpiIntT bVal;          /* bit encoding: ab: 00=0, 10=1, 11=X, 01=Z */
    };

    struct mhpiValueT {
      mhpiValueFormatT format;
      union U {
	char*               str;       /* string value */
	mhpiBitT            bit;        /* mhpi[0,1,X,Z] */
	mhpiIntT            integer;    /* integer value */
	mhpiRealT           realv;       /* real value */
	mhpiTimeT*          time;      /* time value */
	mhpi4LogicVecValT*  logic4Vec; /* 4 logic vector value */
	mhpiBitT*           logic9Vec;  /* 9 logic vector value */
	mhpiCharT           bytev;      /* ...other */
      } 
      U value;
    }/* mhpiValueT*/;

    /*
     * mhpi_initialize
     *   Initialize mhpi routines and specify the hierarchical
     *   pathname delimiter.
     */
    void mhpi_initialize (char path_delimiter);

    /*
     * mhpi_handle_by_name
     *  This function can be used to perform handle searches by name.
     *  Implementation notes:
     *  Implementation will based on mhdlGetObjectByName function call
     */
    mhpiHandleT mhpi_handle_by_name (char* name, mhpiHandleT handle);


    /*
     *  mhpi_get_power_domain_id
     *   This function can be used to query and find out the domain ID
     *   of the scope handle (passed as argument).
     * Implementation notes:
     *   Implementation happesn with virtual function in mhpiScope (with
     *   dummy definition as virtual function).
     *   Actual definition for verilog part in MvpiScope class
     */
    int mhpi_get_power_domain_id(mhpiHandleT handle);

    /*
     * mhpi_get
     *  This function can be used to get integer valued properties. This
     *  function returns 0 on failure, or the value of the property
     */
    mhpiIntT mhpi_get (mhpiIntPropertyT property, mhpiHandleT handle);

    /*************** mhpi_get_str
   This function can be used to return string valued properties.
   The return value is 0 on failure. On success, the string value
   that is returned is expected to copied before a subsequent
   call to this function as the PLI does not guarantee persistence
   across calls
    */

    char* mhpi_get_str(mhpiStrPropertyT property, mhpiHandleT handle);

    /*************** mhpi_handle
   This function can be used to traverse one-to-one relationships.
   The relation and the reference object are passed as parameters.
   If the requested relationship is not supported with respect to
   the class type of the reference handle, then an error will be
   generated
    */

    mhpiHandleT mhpi_handle(mhpiOneToOneT relation, mhpiHandleT handle);

    /*************** mhpi_get_time
   This function set in *time variable current simulation time
   Implementation notes:
    */

    void mhpi_get_time(mhpiTimeT* time);


    /*
     * mhpi_get_vpi_handle
     *  Return VPI subhandle.  Use mhpi_get(mhpiPliP, <handle>)
     *  first to verify that object type is VPI.
     */
    void* mhpi_get_vpi_handle (mhpiHandleT mhpiHandle);

    /* same as mhpi_get_vpi_handle (). Danali request */
    void* mhpi_get_acc_handle (mhpiHandleT mhpiHandle);

    /*
     * mhpi_get_vhpi_handle
     *  Return VHPI subhandle.  Use mhpi_get(mhpiPliP, <handle>)
     *  first to verify that object type is VHPI.
     */
    void* mhpi_get_vhpi_handle (mhpiHandleT mhpiHandle);

    /*
     * mhpi_get_vcsd_handle
     *  Return VCSD subhandle.  Use mhpi_get(mhpiPliP, <handle>)
     *  first to verify that object type is VPI.
     */
    void* mhpi_get_vcsd_handle (mhpiHandleT mhpiHandle);

    /* Return PPI sub-handle. Use mhpi_get(mhpiPliP, <handle>)
     * to check if object type is PPI. NULL will be returned
     * if it is not a PPI.
     */
    void* mhpi_get_ppi_handle (mhpiHandleT mhpiHandle, int findScopeForSupplyTasks);


    /* 
     * mhpi_release_parent_handle
     *  Free memory allocated to the specified mhpi handle.
     *  (Does not free subhandle memory; use corresponding
     *  vpi/vhpi routines for that.)
     */
    void mhpi_release_parent_handle (mhpiHandleT mhpiHandle);

    /*
     * mhpi_iterator
     *  This function can be used to traverse one-to-many relationships.
     *  The relation identifies the relationship and the handle should
     *  have a class type that supports that relationship
     */

    /*************** mhpi_release_handle
   This function should be used to release a PLI handle. All handles
   are created by the PLI and should be released by the users to
   prevent potential memory leaks
   Implementation notes:
     - vhpi_release_handle if handle points to VHDL object
     - vpi_free_object if handle points to Verilog object
    */

    int mhpi_release_handle(mhpiHandleT handle);

    mhpiHandleT mhpi_iterator(mhpiOneToManyT relation, mhpiHandleT handle);

    /* i > 0 means filter all LP objects, 0 means does not filter any LP objects */
    void mhpi_iterator_SetLPFilter(int i);
    /* return > 0 means filter all LP objects, 0 means does not filter any LP objects */
    int mhpi_iterator_GetLPFilter();

    int mhpi_GetLPDesignFlag();
    int mhpi_GetCpfDesignFlag();

    /*
     * mhpi_fileLineInstIterator
     *  This function can be used to traverse one-to-many relationships
     *  of a file/line pair to instances
     */
    mhpiHandleT mhpi_fileLineInstIterator (const char *file, uint line);

    /*
     * mhpi_scan
     *  This function can be used to scan through an iterator. The iterator
     *  creation and this function constitute the PLI mechanism to traverse
     *  one-to-many relationships
     */
    mhpiHandleT mhpi_scan(mhpiHandleT iterator);


    /*
     *  Force/Release classes were moved from ucli so they could
     *  support both ucli and $hdl_xmr_force/relesse
     */

    /* Put Value Flags. The string literals will change with grand unification
       after the standard is balloted */

    enum mhpiPutValueFlagsT {
      mhpiInertialDelay,
      mhpiTransportDelay,
      mhpiPureTransportDelay,
      mhpiNoDelay, /*xmr deposit*/
      mhpiForce,  /*xmr force*/
      mhpiRelease,
      mhpiCancelEvent,
      mhpiDrive   /*xmr drive*/
    };

    /* value for each type of error return */
    enum mhpiReturnT {
      mhpiRetOk = 0,
      mhpiSigPathErr = 1,
      mhpiForceObjCreateErr = 2,
      mhpiTimeValueErr =3,
      mhpiNothingReleasedErr =4,
      mhpiNotVhpiVpiErr =5,
      mhpiDataValueErr =6,
      mhpiCancelDelayErr = 7,
      mhpiRepeatPeriodErr = 8,
      mhpiNodeErr = 9
    };

    /* General Interferface designed for a single force
     */

    mhpiReturnT mhpi_force_value (char* sigPathName, mhpiHandleT curScopeHdl,
				  char* value, mhpiPutValueFlagsT flags,
				  mhpiRealT forceDelay, mhpiRealT cancelDelay);

    /* mhpi_force_multi currently only be used for the UCLI interface
     */

    /** structure to hold time value pairs **/
    struct mhpiForceTimeValue{
      char * value;
      mhpiRealT forceDelay;
    }  /*mhpiForceTimeValue;

/* the function takes a list of times and values to allow multi forces on on signals*/
    mhpiReturnT mhpi_force_multi (char* sigPathName, mhpiHandleT curScopeHdl,
				  mhpiForceTimeValue * valueArray, int numValues,
				  mhpiPutValueFlagsT flags,
				  mhpiRealT cancelDelay,
				  mhpiRealT repeatPeriod);


    /* Used by both ucli release and $hdl_xmr_release */
    mhpiReturnT mhpi_release_force (char* sigPathName, mhpiHandleT curScopeHdl);


    /* mhpi_get_force_return_strings returns error and path stings from mhpi_force_value,
     *  mhpi_force_multi, or mhpi_release_force
     * The function only returns message strings when additional detail is available
     * The objPath string is set to the full path name if a valid object handle is returned
     * The strings are reset to null string at the start of mhpi_force_value, mhpi_force_multi,
     * or mhpi_release_force function call
     * Both messageString and objPath should always be tested to be sure they are not NULL
     */

    struct mhpiForceReturnStrings{
      char * messageString;
      char * objPath;
    };

    mhpiForceReturnStrings * mhpi_get_force_return_stings();

    /*
     * mhpi_fsdb_init() will initialize DKI to be ready for fsdb dumping. Call this
     * function before doing fsdb dumping is mandatory. And it will reserve DKI for
     * fsdb dumping only. So call mhpi_fsdb_reset() to free DKI reservation for
     * fsdb dumping is also mandatory.
     */
    void mhpi_fsdb_init();

    /*
     * mhpi_fsdb_reset() free the DKI reservation for fsdb dumping. See also
     * mhpi_fsdb_init().
     */
    void mhpi_fsdb_reset();

    mhpiHandleT mhpi_handle_from_vhpi (void* vhpiHdl);
    mhpiHandleT mhpi_handle_from_vpi (void* vpiHdl);
    mhpiHandleT mhpi_handle_from_ppi (void* sourceObjHan, void* han, uint isDriver, uint isVerilogHDL);
    mhpiHandleT mhpi_handle_from_vcsd (void* vcsdHdl);

    /*
     * mhpiUcli function parameters.
     */
    alias void* mhpiUcliClientData;
    alias mhpiUcliCmdDeleteProc = void function (mhpiUcliClientData data);
    alias mhpiUcliCmdProc = int function (mhpiUcliClientData data, int argc, char* argv[]);

    /*
     * This function creates a ucli tcl command.
     */
    extern int mhpi_ucliCreateCommand(char *cmdName, mhpiUcliCmdProc cmdProc, mhpiUcliClientData clientData,
				      mhpiUcliCmdDeleteProc deleteProc);

    /*
     * This functions deletes a ucli tcl command.
     */
    extern int mhpi_ucliDeleteCommand(char *cmdName);

    /*
     * This functions sets output result for ucli tcl command.
     */
    extern int mhpi_ucliSetResult(char *result);

    /*
     *   Returns mhpi handle to the current ucli scope
     */
    mhpiHandleT  mhpi_ucliCmdGetCurScopeHandle();

    /*
     * Function to determine if UCLI is enabled
     * Return 1 if in UCLI mode
     * Return 0 if in old CLI/UI mode
     * Supports pure Vlog/MX/pure VHDL
     */
    int mhpi_is_ucli_enabled();

    /*
     * Function to execute tcl command from the pli
     * Returns the return value of the tcl command .
     * Second version mhpi_ucliTclExecNoPrint doesn't dump the output of the tcl command
     * to the console.
     */
    char* mhpi_ucliTclExec(char* cmd);
    char* mhpi_ucliTclExecNoPrint(char* cmd);

    enum mhpiCheckpointOpT {
      mhpiAddCheckpoint = 0,
      mhpiJoinCheckpoint,
      mhpiKillCheckpoint,
      mhpiCheckpiontOpNum /* operation number, always the last one */
    };

    enum mhpiCheckpointCallT {
      mhpiPreCheckpoint = 0,
      mhpiPostCheckpoint
    };

    /*
     * Function to add Callback functions  needs to be called before/after addition/Joining of checkpoint.
     * The signature of the callback is two integers,the first one indicates the checkpoint id
     * to be created or just created(for pre and post) and the current simulation time
     */
    alias checkpointCB = void function (uint chkptId, uint curSimTime);

    int mhpi_addCheckpointCB(checkpointCB cb, mhpiCheckpointOpT op, mhpiCheckpointCallT call) ;
    int mhpi_delCheckpointCB(checkpointCB cb, mhpiCheckpointOpT op, mhpiCheckpointCallT call) ;

    /*
     * In UCLI mode:
     * Returns the type of debugging session.
     * Supports pure Vlog/MX/pure VHDL
     */
    enum mhpiProcessKindT {
      mhpi_PT_NO_DEBUG=0, /* simv running without UCLI/DVE */
      mhpi_PT_ONE,        /* simv running in one process mode with UCLI */
      mhpi_PT_TWO_UCLI,   /* simv running in two process mode with UCLI */
      mhpi_PT_TWO_DVE     /* simv running in two process mode with DVE */
    };

    mhpiProcessKindT mhpi_vcsUcliProcessMode ();

  } /* end extern "C" */
}
