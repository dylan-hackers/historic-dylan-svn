Module:    macos-interface
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file is automatically generated from "Menus.h"; do not edit.

// unnamed enum:
define inline-only constant $kMenuStdMenuProc          = 63;
define inline-only constant $kMenuStdMenuBarProc       = 63;

// unnamed enum:
define inline-only constant $kMenuNoModifiers          = 0;
define inline-only constant $kMenuShiftModifier        = ash(1,0);
define inline-only constant $kMenuOptionModifier       = ash(1,1);
define inline-only constant $kMenuControlModifier      = ash(1,2);
define inline-only constant $kMenuNoCommandModifier    = ash(1,3);

// unnamed enum:
define inline-only constant $kMenuNoIcon               = 0;
define inline-only constant $kMenuIconType             = 1;
define inline-only constant $kMenuShrinkIconType       = 2;
define inline-only constant $kMenuSmallIconType        = 3;
define inline-only constant $kMenuColorIconType        = 4;
define inline-only constant $kMenuIconSuiteType        = 5;
define inline-only constant $kMenuIconRefType          = 6;

// unnamed enum:
define inline-only constant $noMark                    = 0;

// unnamed enum:
define inline-only constant $kMenuDrawMsg              = 0;
define inline-only constant $kMenuChooseMsg            = 1;
define inline-only constant $kMenuSizeMsg              = 2;
define inline-only constant $kMenuDrawItemMsg          = 4;
define inline-only constant $kMenuCalcItemMsg          = 5;
define inline-only constant $kMenuThemeSavvyMsg        = 7;
define inline-only constant $mDrawMsg                  = 0;
define inline-only constant $mChooseMsg                = 1;
define inline-only constant $mSizeMsg                  = 2;
define inline-only constant $mDrawItemMsg              = 4;
define inline-only constant $mCalcItemMsg              = 5;

// unnamed enum:
define inline-only constant $kThemeSavvyMenuResponse   = #x7473;

// unnamed enum:
define inline-only constant $textMenuProc              = 0;
define inline-only constant $hMenuCmd                  = 27;
define inline-only constant $hierMenu                  = -1;
define inline-only constant $mPopUpMsg                 = 3;
define inline-only constant $mctAllItems               = -98;
define inline-only constant $mctLastIDIndic            = -99;


define C-struct <MenuInfo>
  sealed inline-only slot menuID-value   :: <C-short>;
  sealed inline-only slot menuWidth-value :: <C-short>;
  sealed inline-only slot menuHeight-value :: <C-short>;
  sealed inline-only slot menuProc-value :: <Handle>;
  sealed inline-only slot enableFlags-value :: <C-both-long>;
  sealed inline-only slot menuData-value :: <Str255>;
  pack: 2;
  c-name: "struct MenuInfo";
end;
define C-pointer-type <MenuInfo*> => <MenuInfo>;
define C-pointer-type <MenuInfo**> => <MenuInfo*>;
define C-pointer-type <MenuPtr> => <MenuInfo>;
define C-pointer-type <MenuHandle> => <MenuPtr>;
define inline constant <MenuRef> = <MenuHandle>;
define C-pointer-type <MenuRef*> => <MenuRef>;
define C-pointer-type <MenuRef**> => <MenuRef*>;

define C-struct <MCEntry>
  sealed inline-only slot mctID-value    :: <C-short>;
  sealed inline-only slot mctItem-value  :: <C-short>;
  sealed inline-only slot mctRGB1-value  :: <RGBColor>;
  sealed inline-only slot mctRGB2-value  :: <RGBColor>;
  sealed inline-only slot mctRGB3-value  :: <RGBColor>;
  sealed inline-only slot mctRGB4-value  :: <RGBColor>;
  sealed inline-only slot mctReserved    :: <C-short>;
  pack: 2;
  c-name: "struct MCEntry";
end;
define C-pointer-type <MCEntry*> => <MCEntry>;
define C-pointer-type <MCEntry**> => <MCEntry*>;
define C-pointer-type <MCEntryPtr> => <MCEntry>;
define C-pointer-type <MCTablePtr> => <MCEntry>;
define C-pointer-type <MCTableHandle> => <MCTablePtr>;

define C-struct <MenuCRsrc>
  sealed inline-only slot numEntries-value :: <C-short>;
  sealed inline-only slot mcEntryRecs-value :: <MCTable>;
  pack: 2;
  c-name: "struct MenuCRsrc";
end;
define C-pointer-type <MenuCRsrc*> => <MenuCRsrc>;
define C-pointer-type <MenuCRsrc**> => <MenuCRsrc*>;
define C-pointer-type <MenuCRsrcPtr> => <MenuCRsrc>;
define C-pointer-type <MenuCRsrcHandle> => <MenuCRsrcPtr>;
define constant <MenuDefProcPtr> = <C-function-pointer>;
define constant <MenuBarDefProcPtr> = <C-function-pointer>;
define constant <MenuHookProcPtr> = <C-function-pointer>;
define constant <MBarHookProcPtr> = <C-function-pointer>;
define constant <MenuDefUPP> = <UniversalProcPtr>;
define constant <MenuBarDefUPP> = <UniversalProcPtr>;
define constant <MenuHookUPP> = <UniversalProcPtr>;
define constant <MBarHookUPP> = <UniversalProcPtr>;
// unnamed enum:
define inline-only constant $uppMenuDefProcInfo        = #x0000FF80;

// unnamed enum:
define inline-only constant $uppMenuBarDefProcInfo     = #x00003AB0;

// unnamed enum:
define inline-only constant $uppMenuHookProcInfo       = #x00000000;

// unnamed enum:
define inline-only constant $uppMBarHookProcInfo       = #x000000CF;


define inline-only C-function GetMBarHeight
  result value :: <C-short>;
  c-name: "GetMBarHeight";
  c-modifiers: "pascal";
end;

define inline-only C-function InitMenus
  c-name: "InitMenus";
  c-modifiers: "pascal";
end;

define inline-only C-function NewMenu
  parameter menuID     :: <C-short>;
  parameter menuTitle  :: <ConstStr255Param>;
  result value :: <MenuHandle>;
  c-name: "NewMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function MacGetMenu
  parameter resourceID :: <C-short>;
  result value :: <MenuHandle>;
  c-name: "MacGetMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function DisposeMenu
  parameter theMenu    :: <MenuHandle>;
  c-name: "DisposeMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function MacAppendMenu
  parameter menu       :: <MenuHandle>;
  parameter data       :: <ConstStr255Param>;
  c-name: "MacAppendMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function InsertResMenu
  parameter theMenu    :: <MenuHandle>;
  parameter theType    :: <ResType>;
  parameter afterItem  :: <C-short>;
  c-name: "InsertResMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function MacInsertMenu
  parameter theMenu    :: <MenuHandle>;
  parameter beforeID   :: <C-short>;
  c-name: "MacInsertMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function MacDeleteMenu
  parameter menuID     :: <C-short>;
  c-name: "MacDeleteMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function AppendResMenu
  parameter theMenu    :: <MenuHandle>;
  parameter theType    :: <ResType>;
  c-name: "AppendResMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function MacInsertMenuItem
  parameter theMenu    :: <MenuHandle>;
  parameter itemString :: <ConstStr255Param>;
  parameter afterItem  :: <C-short>;
  c-name: "MacInsertMenuItem";
  c-modifiers: "pascal";
end;

define inline-only C-function DeleteMenuItem
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  c-name: "DeleteMenuItem";
  c-modifiers: "pascal";
end;

define inline-only C-function MenuKey
  parameter ch         :: <CharParameter>;
  result value :: <C-both-long>;
  c-name: "MenuKey";
  c-modifiers: "pascal";
end;

define inline-only C-function HiliteMenu
  parameter menuID     :: <C-short>;
  c-name: "HiliteMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemText
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter itemString :: <ConstStr255Param>;
  c-name: "SetMenuItemText";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemText
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter itemString :: <Str255>;
  c-name: "GetMenuItemText";
  c-modifiers: "pascal";
end;

define inline-only C-function SetItemMark
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter markChar   :: <CharParameter>;
  c-name: "SetItemMark";
  c-modifiers: "pascal";
end;

define inline-only C-function GetItemMark
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter markChar   :: <CharParameter*>;
  c-name: "GetItemMark";
  c-modifiers: "pascal";
end;

define inline-only C-function SetItemCmd
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter cmdChar    :: <CharParameter>;
  c-name: "SetItemCmd";
  c-modifiers: "pascal";
end;

define inline-only C-function GetItemCmd
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter cmdChar    :: <CharParameter*>;
  c-name: "GetItemCmd";
  c-modifiers: "pascal";
end;

define inline-only C-function SetItemIcon
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter iconIndex  :: <C-short>;
  c-name: "SetItemIcon";
  c-modifiers: "pascal";
end;

define inline-only C-function GetItemIcon
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter iconIndex  :: <C-short*>;
  c-name: "GetItemIcon";
  c-modifiers: "pascal";
end;

define inline-only C-function SetItemStyle
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter chStyle    :: <StyleParameter>;
  c-name: "SetItemStyle";
  c-modifiers: "pascal";
end;

define inline-only C-function GetItemStyle
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter chStyle    :: <Style*>;
  c-name: "GetItemStyle";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuHandle
  parameter menuID     :: <C-short>;
  result value :: <MenuHandle>;
  c-name: "GetMenuHandle";
  c-modifiers: "pascal";
end;

define inline-only C-function CalcMenuSize
  parameter theMenu    :: <MenuHandle>;
  c-name: "CalcMenuSize";
  c-modifiers: "pascal";
end;

define inline-only C-function DisableItem
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  c-name: "DisableItem";
  c-modifiers: "pascal";
end;

define inline-only C-function EnableItem
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  c-name: "EnableItem";
  c-modifiers: "pascal";
end;

define inline-only C-function FlashMenuBar
  parameter menuID     :: <C-short>;
  c-name: "FlashMenuBar";
  c-modifiers: "pascal";
end;

define inline-only C-function PopUpMenuSelect
  parameter menu       :: <MenuHandle>;
  parameter top        :: <C-short>;
  parameter left       :: <C-short>;
  parameter popUpItem  :: <C-short>;
  result value :: <C-both-long>;
  c-name: "PopUpMenuSelect";
  c-modifiers: "pascal";
end;

define inline-only C-function MenuChoice
  result value :: <C-both-long>;
  c-name: "MenuChoice";
  c-modifiers: "pascal";
end;

define inline-only C-function DeleteMCEntries
  parameter menuID     :: <C-short>;
  parameter menuItem   :: <C-short>;
  c-name: "DeleteMCEntries";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMCInfo
  result value :: <MCTableHandle>;
  c-name: "GetMCInfo";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMCInfo
  parameter menuCTbl   :: <MCTableHandle>;
  c-name: "SetMCInfo";
  c-modifiers: "pascal";
end;

define inline-only C-function DisposeMCInfo
  parameter menuCTbl   :: <MCTableHandle>;
  c-name: "DisposeMCInfo";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMCEntry
  parameter menuID     :: <C-short>;
  parameter menuItem   :: <C-short>;
  result value :: <MCEntryPtr>;
  c-name: "GetMCEntry";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMCEntries
  parameter numEntries :: <C-short>;
  parameter menuCEntries :: <MCTablePtr>;
  c-name: "SetMCEntries";
  c-modifiers: "pascal";
end;

define inline-only C-function MacDrawMenuBar
  c-name: "MacDrawMenuBar";
  c-modifiers: "pascal";
end;

define inline-only C-function InvalMenuBar
  c-name: "InvalMenuBar";
  c-modifiers: "pascal";
end;

define inline-only C-function InitProcMenu
  parameter resID      :: <C-short>;
  c-name: "InitProcMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuBar
  result value :: <Handle>;
  c-name: "GetMenuBar";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuBar
  parameter menuList   :: <Handle>;
  c-name: "SetMenuBar";
  c-modifiers: "pascal";
end;

define inline-only C-function SystemEdit
  parameter editCmd    :: <C-short>;
  result value :: <MacBoolean>;
  c-name: "SystemEdit";
  c-modifiers: "pascal";
end;

define inline-only C-function SystemMenu
  parameter menuResult :: <C-both-long>;
  c-name: "SystemMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function GetNewMBar
  parameter menuBarID  :: <C-short>;
  result value :: <Handle>;
  c-name: "GetNewMBar";
  c-modifiers: "pascal";
end;

define inline-only C-function ClearMenuBar
  c-name: "ClearMenuBar";
  c-modifiers: "pascal";
end;

define inline-only C-function CheckItem
  parameter theMenu    :: <MenuHandle>;
  parameter item       :: <C-short>;
  parameter checked    :: <MacBoolean>;
  c-name: "CheckItem";
  c-modifiers: "pascal";
end;

define inline-only C-function CountMItems
  parameter theMenu    :: <MenuHandle>;
  result value :: <C-short>;
  c-name: "CountMItems";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuFlash
  parameter count      :: <C-short>;
  c-name: "SetMenuFlash";
  c-modifiers: "pascal";
end;

define inline-only C-function MenuSelect
  parameter startPt    :: <Point>;
  result value :: <C-both-long>;
  c-name: "MenuSelect";
  c-modifiers: "pascal";
end;

define inline-only C-function InsertFontResMenu
  parameter theMenu    :: <MenuHandle>;
  parameter afterItem  :: <C-short>;
  parameter scriptFilter :: <C-short>;
  c-name: "InsertFontResMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function InsertIntlResMenu
  parameter theMenu    :: <MenuHandle>;
  parameter theType    :: <ResType>;
  parameter afterItem  :: <C-short>;
  parameter scriptFilter :: <C-short>;
  c-name: "InsertIntlResMenu";
  c-modifiers: "pascal";
end;

define inline-only C-function MenuEvent
  parameter inEvent    ::  /* const */ <EventRecord*>;
  result value :: <UInt32>;
  c-name: "MenuEvent";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemCommandID
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inCommandID :: <UInt32>;
  result value :: <OSErr>;
  c-name: "SetMenuItemCommandID";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemCommandID
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outCommandID :: <UInt32*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemCommandID";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemModifiers
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inModifiers :: <UInt8>;
  result value :: <OSErr>;
  c-name: "SetMenuItemModifiers";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemModifiers
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outModifiers :: <UInt8*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemModifiers";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemIconHandle
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inIconType :: <UInt8>;
  parameter inIconHandle :: <Handle>;
  result value :: <OSErr>;
  c-name: "SetMenuItemIconHandle";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemIconHandle
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outIconType :: <UInt8*>;
  parameter outIconHandle :: <Handle*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemIconHandle";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemTextEncoding
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inScriptID :: <TextEncoding>;
  result value :: <OSErr>;
  c-name: "SetMenuItemTextEncoding";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemTextEncoding
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outScriptID :: <TextEncoding*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemTextEncoding";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemHierarchicalID
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inHierID   :: <SInt16>;
  result value :: <OSErr>;
  c-name: "SetMenuItemHierarchicalID";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemHierarchicalID
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outHierID  :: <SInt16*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemHierarchicalID";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemFontID
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inFontID   :: <SInt16>;
  result value :: <OSErr>;
  c-name: "SetMenuItemFontID";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemFontID
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outFontID  :: <SInt16*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemFontID";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemRefCon
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inRefCon   :: <UInt32>;
  result value :: <OSErr>;
  c-name: "SetMenuItemRefCon";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemRefCon
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outRefCon  :: <UInt32*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemRefCon";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemRefCon2
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inRefCon2  :: <UInt32>;
  result value :: <OSErr>;
  c-name: "SetMenuItemRefCon2";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemRefCon2
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outRefCon2 :: <UInt32*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemRefCon2";
  c-modifiers: "pascal";
end;

define inline-only C-function SetMenuItemKeyGlyph
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter inGlyph    :: <SInt16>;
  result value :: <OSErr>;
  c-name: "SetMenuItemKeyGlyph";
  c-modifiers: "pascal";
end;

define inline-only C-function GetMenuItemKeyGlyph
  parameter inMenu     :: <MenuHandle>;
  parameter inItem     :: <SInt16>;
  parameter outGlyph   :: <SInt16*>;
  result value :: <OSErr>;
  c-name: "GetMenuItemKeyGlyph";
  c-modifiers: "pascal";
end;
// unnamed enum:
define inline-only constant $gestaltContextualMenuAttr = FOUR_CHAR_CODE('c', 'm', 'n', 'u');
define inline-only constant $gestaltContextualMenuUnusedBit = 0;
define inline-only constant $gestaltContextualMenuTrapAvailable = 1;

// unnamed enum:
define inline-only constant $kCMHelpItemNoHelp         = 0;
define inline-only constant $kCMHelpItemAppleGuide     = 1;
define inline-only constant $kCMHelpItemOtherHelp      = 2;

// unnamed enum:
define inline-only constant $kCMNothingSelected        = 0;
define inline-only constant $kCMMenuItemSelected       = 1;
define inline-only constant $kCMShowHelpSelected       = 3;


define inline-only C-function InitContextualMenus
  result value :: <OSStatus>;
  c-name: "InitContextualMenus";
  c-modifiers: "pascal";
end;

define inline-only C-function IsShowContextualMenuClick
  parameter inEvent    ::  /* const */ <EventRecord*>;
  result value :: <MacBoolean>;
  c-name: "IsShowContextualMenuClick";
  c-modifiers: "pascal";
end;

define inline-only C-function ContextualMenuSelect
  parameter inMenu     :: <MenuHandle>;
  parameter inGlobalLocation :: <Point>;
  parameter inReserved :: <MacBoolean>;
  parameter inHelpType :: <UInt32>;
  parameter inHelpItemString :: <ConstStr255Param>;
  parameter inSelection ::  /* const */ <AEDesc*>;
  parameter outUserSelectionType :: <UInt32*>;
  parameter outMenuID  :: <SInt16*>;
  parameter outMenuItem :: <UInt16*>;
  result value :: <OSStatus>;
  c-name: "ContextualMenuSelect";
  c-modifiers: "pascal";
end;

define inline-only C-function ProcessIsContextualMenuClient
  parameter inPSN      :: <ProcessSerialNumber*>;
  result value :: <MacBoolean>;
  c-name: "ProcessIsContextualMenuClient";
  c-modifiers: "pascal";
end;

