Module:    carbon-interface
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file is automatically generated from "OSUtils.h"; do not edit.

// unnamed enum:
define inline-only constant $useFree                   = 0;
define inline-only constant $useATalk                  = 1;
define inline-only constant $useAsync                  = 2;
define inline-only constant $useExtClk                 = 3;
define inline-only constant $useMIDI                   = 4;

// unnamed enum:
define inline-only constant $false32b                  = 0;
define inline-only constant $true32b                   = 1;

// unnamed enum:
define inline-only constant $sortsBefore               = -1;
define inline-only constant $sortsEqual                = 0;
define inline-only constant $sortsAfter                = 1;

// unnamed enum:
define inline-only constant $dummyType                 = 0;
define inline-only constant $vType                     = 1;
define inline-only constant $ioQType                   = 2;
define inline-only constant $drvQType                  = 3;
define inline-only constant $evType                    = 4;
define inline-only constant $fsQType                   = 5;
define inline-only constant $sIQType                   = 6;
define inline-only constant $dtQType                   = 7;
define inline-only constant $nmType                    = 8;

define inline constant <QTypes> = <SignedByte>;
define C-pointer-type <QTypes*> => <QTypes>;
define C-pointer-type <QTypes**> => <QTypes*>;

define C-struct <SysParmType>
  sealed inline-only slot valid-value    :: <UInt8>;
  sealed inline-only slot aTalkA-value   :: <UInt8>;
  sealed inline-only slot aTalkB-value   :: <UInt8>;
  sealed inline-only slot config-value   :: <UInt8>;
  sealed inline-only slot portA-value    :: <C-short>;
  sealed inline-only slot portB-value    :: <C-short>;
  sealed inline-only slot alarm-value    :: <C-both-long>;
  sealed inline-only slot font-value     :: <C-short>;
  sealed inline-only slot kbdPrint-value :: <C-short>;
  sealed inline-only slot volClik-value  :: <C-short>;
  sealed inline-only slot misc-value     :: <C-short>;
  pack: 2;
  c-name: "struct SysParmType";
end;
define C-pointer-type <SysParmType*> => <SysParmType>;
define C-pointer-type <SysParmType**> => <SysParmType*>;
define C-pointer-type <SysPPtr> => <SysParmType>;

define C-struct <QElem>
  sealed inline-only slot qLink-value    :: <QElem*>;
  sealed inline-only slot qType-value    :: <C-short>;
  sealed inline-only array slot qData-array :: <C-short>,
    length: 1,
    address-getter: qData-value;
  pack: 2;
  c-name: "struct QElem";
end;
define C-pointer-type <QElem*> => <QElem>;
define C-pointer-type <QElem**> => <QElem*>;
define C-pointer-type <QElemPtr> => <QElem>;

define C-struct <QHdr>
  sealed inline-only slot qFlags-value   :: <C-short>;
  sealed inline-only slot qHead-value    :: <QElemPtr>;
  sealed inline-only slot qTail-value    :: <QElemPtr>;
  pack: 2;
  c-name: "struct QHdr";
end;
define C-pointer-type <QHdr*> => <QHdr>;
define C-pointer-type <QHdr**> => <QHdr*>;
define C-pointer-type <QHdrPtr> => <QHdr>;
define constant <DeferredTaskProcPtr> = <C-function-pointer>;
define constant <DeferredTaskUPP> = <UniversalProcPtr>;
// unnamed enum:
define inline-only constant $uppDeferredTaskProcInfo   = #x0000B802;


define C-struct <DeferredTask>
  sealed inline-only slot qLink-value    :: <QElemPtr>;
  sealed inline-only slot qType-value    :: <C-short>;
  sealed inline-only slot dtFlags-value  :: <C-short>;
  sealed inline-only slot dtAddr-value   :: <DeferredTaskUPP>;
  sealed inline-only slot dtParam-value  :: <C-both-long>;
  sealed inline-only slot dtReserved     :: <C-both-long>;
  pack: 2;
  c-name: "struct DeferredTask";
end;
define C-pointer-type <DeferredTask*> => <DeferredTask>;
define C-pointer-type <DeferredTask**> => <DeferredTask*>;
define C-pointer-type <DeferredTaskPtr> => <DeferredTask>;
define C-union <MachineLocation%u%1>
  sealed inline-only slot dlsDelta-value :: <SInt8>;
  sealed inline-only slot gmtDelta-value :: <C-both-long>;
  pack: 2;
end;

define C-struct <MachineLocation>
  sealed inline-only slot latitude-value :: <Fract>;
  sealed inline-only slot longitude-value :: <Fract>;
  sealed inline-only slot u-value        :: <MachineLocation%u%1>;
  pack: 2;
  c-name: "struct MachineLocation";
end;
define C-pointer-type <MachineLocation*> => <MachineLocation>;
define C-pointer-type <MachineLocation**> => <MachineLocation*>;

define inline-only C-function IsMetric
  result value :: <MacBoolean>;
  c-name: "IsMetric";
  c-modifiers: "pascal";
end;

define inline-only C-function GetSysPPtr
  result value :: <SysPPtr>;
  c-name: "GetSysPPtr";
  c-modifiers: "pascal";
end;

define inline-only C-function DTInstall
  parameter dtTaskPtr  :: <DeferredTaskPtr>;
  result value :: <OSErr>;
  c-name: "DTInstall";
  c-modifiers: "pascal";
end;

define inline-only C-function Delay
  parameter numTicks   :: <C-both-unsigned-long>;
  parameter finalTicks :: <C-both-unsigned-long*>;
  c-name: "Delay";
  c-modifiers: "pascal";
end;

define inline-only C-function WriteParam
  result value :: <OSErr>;
  c-name: "WriteParam";
  c-modifiers: "pascal";
end;

define inline-only C-function Enqueue
  parameter qElement   :: <QElemPtr>;
  parameter qHeader    :: <QHdrPtr>;
  c-name: "Enqueue";
  c-modifiers: "pascal";
end;

define inline-only C-function Dequeue
  parameter qElement   :: <QElemPtr>;
  parameter qHeader    :: <QHdrPtr>;
  result value :: <OSErr>;
  c-name: "Dequeue";
  c-modifiers: "pascal";
end;

define inline-only C-function SetCurrentA5
  result value :: <C-both-long>;
  c-name: "SetCurrentA5";
  c-modifiers: "pascal";
end;

define inline-only C-function SetA5
  parameter newA5      :: <C-both-long>;
  result value :: <C-both-long>;
  c-name: "SetA5";
  c-modifiers: "pascal";
end;

define inline-only C-function InitUtil
  result value :: <OSErr>;
  c-name: "InitUtil";
  c-modifiers: "pascal";
end;

define inline-only C-function MakeDataExecutable
  parameter baseAddress :: <C-void*>;
  parameter length     :: <C-both-unsigned-long>;
  c-name: "MakeDataExecutable";
  c-modifiers: "pascal";
end;

define inline-only C-function FlushCodeCacheRange
  parameter address    :: <C-void*>;
  parameter count      :: <C-both-unsigned-long>;
  result value :: <OSErr>;
  c-name: "FlushCodeCacheRange";
  c-modifiers: "pascal";
end;

define inline-only C-function ReadLocation
  parameter loc        :: <MachineLocation*>;
  c-name: "ReadLocation";
  c-modifiers: "pascal";
end;

define inline-only C-function WriteLocation
  parameter loc        ::  /* const */ <MachineLocation*>;
  c-name: "WriteLocation";
  c-modifiers: "pascal";
end;
// unnamed enum:
define inline-only constant $curSysEnvVers             = 2;


define C-struct <SysEnvRec>
  sealed inline-only slot environsVersion-value :: <C-short>;
  sealed inline-only slot machineType-value :: <C-short>;
  sealed inline-only slot systemVersion-value :: <C-short>;
  sealed inline-only slot processor-value :: <C-short>;
  sealed inline-only slot hasFPU-value   :: <MacBoolean>;
  sealed inline-only slot hasColorQD-value :: <MacBoolean>;
  sealed inline-only slot keyBoardType-value :: <C-short>;
  sealed inline-only slot atDrvrVersNum-value :: <C-short>;
  sealed inline-only slot sysVRefNum-value :: <C-short>;
  pack: 2;
  c-name: "struct SysEnvRec";
end;
define C-pointer-type <SysEnvRec*> => <SysEnvRec>;
define C-pointer-type <SysEnvRec**> => <SysEnvRec*>;
// unnamed enum:
define inline-only constant $envMac                    = -1;
define inline-only constant $envXL                     = -2;
define inline-only constant $envMachUnknown            = 0;
define inline-only constant $env512KE                  = 1;
define inline-only constant $envMacPlus                = 2;
define inline-only constant $envSE                     = 3;
define inline-only constant $envMacII                  = 4;
define inline-only constant $envMacIIx                 = 5;
define inline-only constant $envMacIIcx                = 6;
define inline-only constant $envSE30                   = 7;
define inline-only constant $envPortable               = 8;
define inline-only constant $envMacIIci                = 9;
define inline-only constant $envMacIIfx                = 11;

// unnamed enum:
define inline-only constant $envCPUUnknown             = 0;
define inline-only constant $env68000                  = 1;
define inline-only constant $env68010                  = 2;
define inline-only constant $env68020                  = 3;
define inline-only constant $env68030                  = 4;
define inline-only constant $env68040                  = 5;

// unnamed enum:
define inline-only constant $envUnknownKbd             = 0;
define inline-only constant $envMacKbd                 = 1;
define inline-only constant $envMacAndPad              = 2;
define inline-only constant $envMacPlusKbd             = 3;
define inline-only constant $envAExtendKbd             = 4;
define inline-only constant $envStandADBKbd            = 5;
define inline-only constant $envPrtblADBKbd            = 6;
define inline-only constant $envPrtblISOKbd            = 7;
define inline-only constant $envStdISOADBKbd           = 8;
define inline-only constant $envExtISOADBKbd           = 9;


define inline-only C-function SysEnvirons
  parameter versionRequested :: <C-short>;
  parameter theWorld   :: <SysEnvRec*>;
  result value :: <OSErr>;
  c-name: "SysEnvirons";
  c-modifiers: "pascal";
end;

