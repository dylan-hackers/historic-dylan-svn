Module: type-library-module
Creator: created from "E:\projects\dylanlibs\dylanlibs\win32\ms-internet\type-library.spec" at 21:50 2001- 5- 1 New Zealand Standard Time.


/* Type library: SHDocVw version 1.1
 * Description: Microsoft Internet Controls
 * GUID: {EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}
 */


/* hidden? Dispatch interface: IWebBrowser version 0.0
 * GUID: {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
 * Description: Web Browser interface
 */
define dispatch-client <IWebBrowser>
  uuid "{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}";

  /* Navigates to the previous item in the history list. */
  function IWebBrowser/GoBack () => (), name: "GoBack", disp-id: 100;

  /* Navigates to the next item in the history list. */
  function IWebBrowser/GoForward () => (), name: "GoForward", disp-id: 101;

  /* Go home/start page. */
  function IWebBrowser/GoHome () => (), name: "GoHome", disp-id: 102;

  /* Go Search Page. */
  function IWebBrowser/GoSearch () => (), name: "GoSearch", disp-id: 103;

  /* Navigates to a URL or file. */
  function IWebBrowser/Navigate (arg-URL :: <string>, /*optional*/ 
        arg-Flags :: <VARIANT*>, /*optional*/ arg-TargetFrameName :: 
        <VARIANT*>, /*optional*/ arg-PostData :: <VARIANT*>, /*optional*/ 
        arg-Headers :: <VARIANT*>) => (), name: "Navigate", disp-id: 104;

  /* Refresh the currently viewed page. */
  function IWebBrowser/Refresh () => (), name: "Refresh", disp-id: -550;

  /* Refresh the currently viewed page. */
  function IWebBrowser/Refresh2 (/*optional*/ arg-Level :: <VARIANT*>) => 
        (), name: "Refresh2", disp-id: 105;

  /* Stops opening a file. */
  function IWebBrowser/Stop () => (), name: "Stop", disp-id: 106;

  /* Returns the application automation object if accessible, this 
        automation object otherwise.. */
  constant property IWebBrowser/Application :: <LPDISPATCH>, name: 
        "Application", disp-id: 200;

  /* Returns the automation object of the container/parent if one exists or 
        this automation object. */
  constant property IWebBrowser/Parent :: <LPDISPATCH>, name: "Parent", 
        disp-id: 201;

  /* Returns the container/parent automation object, if any. */
  constant property IWebBrowser/Container :: <LPDISPATCH>, name: 
        "Container", disp-id: 202;

  /* Returns the active Document automation object, if any. */
  constant property IWebBrowser/Document :: <LPDISPATCH>, name: "Document", 
        disp-id: 203;

  /* Returns True if this is the top level object. */
  constant property IWebBrowser/TopLevelContainer :: <boolean>, name: 
        "TopLevelContainer", disp-id: 204;

  /* Returns the type of the contained document object. */
  constant property IWebBrowser/Type :: <string>, name: "Type", disp-id: 
        205;

  /* The horizontal position (pixels) of the frame window relative to the 
        screen/container. */
  property IWebBrowser/Left :: type-union(<integer>, <machine-word>), name: 
        "Left", disp-id: 206;

  /* The vertical position (pixels) of the frame window relative to the 
        screen/container. */
  property IWebBrowser/Top :: type-union(<integer>, <machine-word>), name: 
        "Top", disp-id: 207;

  /* The horizontal dimension (pixels) of the frame window/object. */
  property IWebBrowser/Width :: type-union(<integer>, <machine-word>), 
        name: "Width", disp-id: 208;

  /* The vertical dimension (pixels) of the frame window/object. */
  property IWebBrowser/Height :: type-union(<integer>, <machine-word>), 
        name: "Height", disp-id: 209;

  /* Gets the short (UI-friendly) name of the URL/file currently viewed. */
  constant property IWebBrowser/LocationName :: <string>, name: 
        "LocationName", disp-id: 210;

  /* Gets the full URL/path currently viewed. */
  constant property IWebBrowser/LocationURL :: <string>, name: 
        "LocationURL", disp-id: 211;

  /* Query to see if something is still in progress. */
  constant property IWebBrowser/Busy :: <boolean>, name: "Busy", disp-id: 
        212;
end dispatch-client <IWebBrowser>;


/* hidden? Dispatch interface: DWebBrowserEvents version 0.0
 * GUID: {EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}
 * Description: Web Browser Control Events (old)
 */
define dispatch-client <DWebBrowserEvents>
  uuid "{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}";

  /* Fired when a new hyperlink is being navigated to. */
  function DWebBrowserEvents/BeforeNavigate (arg-URL :: <string>, arg-Flags 
        :: type-union(<integer>, <machine-word>), arg-TargetFrameName :: 
        <string>, arg-PostData :: <VARIANT*>, arg-Headers :: <string>, 
        arg-Cancel :: inout-ref(<VARIANT-BOOL>)) => (), name: 
        "BeforeNavigate", disp-id: 100;

  /* Fired when the document being navigated to becomes visible and enters 
        the navigation stack. */
  function DWebBrowserEvents/NavigateComplete (arg-URL :: <string>) => (), 
        name: "NavigateComplete", disp-id: 101;

  /* Statusbar text changed. */
  function DWebBrowserEvents/StatusTextChange (arg-Text :: <string>) => (), 
        name: "StatusTextChange", disp-id: 102;

  /* Fired when download progress is updated. */
  function DWebBrowserEvents/ProgressChange (arg-Progress :: 
        type-union(<integer>, <machine-word>), arg-ProgressMax :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "ProgressChange", disp-id: 108;

  /* Download of page complete. */
  function DWebBrowserEvents/DownloadComplete () => (), name: 
        "DownloadComplete", disp-id: 104;

  /* The enabled state of a command changed */
  function DWebBrowserEvents/CommandStateChange (arg-Command :: 
        type-union(<integer>, <machine-word>), arg-Enable :: <boolean>) => 
        (), name: "CommandStateChange", disp-id: 105;

  /* Download of a page started. */
  function DWebBrowserEvents/DownloadBegin () => (), name: "DownloadBegin", 
        disp-id: 106;

  /* Fired when a new window should be created. */
  function DWebBrowserEvents/NewWindow (arg-URL :: <string>, arg-Flags :: 
        type-union(<integer>, <machine-word>), arg-TargetFrameName :: 
        <string>, arg-PostData :: <VARIANT*>, arg-Headers :: <string>, 
        arg-Processed :: inout-ref(<VARIANT-BOOL>)) => (), name: 
        "NewWindow", disp-id: 107;

  /* Document title changed. */
  function DWebBrowserEvents/TitleChange (arg-Text :: <string>) => (), 
        name: "TitleChange", disp-id: 113;

  /* Fired when a new hyperlink is being navigated to in a frame. */
  function DWebBrowserEvents/FrameBeforeNavigate (arg-URL :: <string>, 
        arg-Flags :: type-union(<integer>, <machine-word>), 
        arg-TargetFrameName :: <string>, arg-PostData :: <VARIANT*>, 
        arg-Headers :: <string>, arg-Cancel :: inout-ref(<VARIANT-BOOL>)) 
        => (), name: "FrameBeforeNavigate", disp-id: 200;

  /* Fired when a new hyperlink is being navigated to in a frame. */
  function DWebBrowserEvents/FrameNavigateComplete (arg-URL :: <string>) => 
        (), name: "FrameNavigateComplete", disp-id: 201;

  /* Fired when a new window should be created. */
  function DWebBrowserEvents/FrameNewWindow (arg-URL :: <string>, arg-Flags 
        :: type-union(<integer>, <machine-word>), arg-TargetFrameName :: 
        <string>, arg-PostData :: <VARIANT*>, arg-Headers :: <string>, 
        arg-Processed :: inout-ref(<VARIANT-BOOL>)) => (), name: 
        "FrameNewWindow", disp-id: 204;

  /* Fired when application is quiting. */
  function DWebBrowserEvents/Quit (arg-Cancel :: inout-ref(<VARIANT-BOOL>)) 
        => (), name: "Quit", disp-id: 103;

  /* Fired when window has been moved. */
  function DWebBrowserEvents/WindowMove () => (), name: "WindowMove", 
        disp-id: 109;

  /* Fired when window has been sized. */
  function DWebBrowserEvents/WindowResize () => (), name: "WindowResize", 
        disp-id: 110;

  /* Fired when window has been activated. */
  function DWebBrowserEvents/WindowActivate () => (), name: 
        "WindowActivate", disp-id: 111;

  /* Fired when the PutProperty method has been called. */
  function DWebBrowserEvents/PropertyChange (arg-Property :: <string>) => 
        (), name: "PropertyChange", disp-id: 112;
end dispatch-client <DWebBrowserEvents>;


/* Enumeration: CommandStateChangeConstants
 * Description: Constants for WebBrowser CommandStateChange
 */
define constant <CommandStateChangeConstants> = type-union(<integer>, 
        <machine-word>);
define constant $CSC-UPDATECOMMANDS = -1;
define constant $CSC-NAVIGATEFORWARD = 1;
define constant $CSC-NAVIGATEBACK = 2;


/* hidden? Dispatch interface: IWebBrowserApp version 0.0
 * GUID: {0002DF05-0000-0000-C000-000000000046}
 * Description: Web Browser Application Interface.
 */
define dispatch-client <IWebBrowserApp>
  uuid "{0002DF05-0000-0000-C000-000000000046}";

  /* Navigates to the previous item in the history list. */
  function IWebBrowserApp/GoBack () => (), name: "GoBack", disp-id: 100;

  /* Navigates to the next item in the history list. */
  function IWebBrowserApp/GoForward () => (), name: "GoForward", disp-id: 
        101;

  /* Go home/start page. */
  function IWebBrowserApp/GoHome () => (), name: "GoHome", disp-id: 102;

  /* Go Search Page. */
  function IWebBrowserApp/GoSearch () => (), name: "GoSearch", disp-id: 
        103;

  /* Navigates to a URL or file. */
  function IWebBrowserApp/Navigate (arg-URL :: <string>, /*optional*/ 
        arg-Flags :: <VARIANT*>, /*optional*/ arg-TargetFrameName :: 
        <VARIANT*>, /*optional*/ arg-PostData :: <VARIANT*>, /*optional*/ 
        arg-Headers :: <VARIANT*>) => (), name: "Navigate", disp-id: 104;

  /* Refresh the currently viewed page. */
  function IWebBrowserApp/Refresh () => (), name: "Refresh", disp-id: -550;

  /* Refresh the currently viewed page. */
  function IWebBrowserApp/Refresh2 (/*optional*/ arg-Level :: <VARIANT*>) 
        => (), name: "Refresh2", disp-id: 105;

  /* Stops opening a file. */
  function IWebBrowserApp/Stop () => (), name: "Stop", disp-id: 106;

  /* Returns the application automation object if accessible, this 
        automation object otherwise.. */
  constant property IWebBrowserApp/Application :: <LPDISPATCH>, name: 
        "Application", disp-id: 200;

  /* Returns the automation object of the container/parent if one exists or 
        this automation object. */
  constant property IWebBrowserApp/Parent :: <LPDISPATCH>, name: "Parent", 
        disp-id: 201;

  /* Returns the container/parent automation object, if any. */
  constant property IWebBrowserApp/Container :: <LPDISPATCH>, name: 
        "Container", disp-id: 202;

  /* Returns the active Document automation object, if any. */
  constant property IWebBrowserApp/Document :: <LPDISPATCH>, name: 
        "Document", disp-id: 203;

  /* Returns True if this is the top level object. */
  constant property IWebBrowserApp/TopLevelContainer :: <boolean>, name: 
        "TopLevelContainer", disp-id: 204;

  /* Returns the type of the contained document object. */
  constant property IWebBrowserApp/Type :: <string>, name: "Type", disp-id: 
        205;

  /* The horizontal position (pixels) of the frame window relative to the 
        screen/container. */
  property IWebBrowserApp/Left :: type-union(<integer>, <machine-word>), 
        name: "Left", disp-id: 206;

  /* The vertical position (pixels) of the frame window relative to the 
        screen/container. */
  property IWebBrowserApp/Top :: type-union(<integer>, <machine-word>), 
        name: "Top", disp-id: 207;

  /* The horizontal dimension (pixels) of the frame window/object. */
  property IWebBrowserApp/Width :: type-union(<integer>, <machine-word>), 
        name: "Width", disp-id: 208;

  /* The vertical dimension (pixels) of the frame window/object. */
  property IWebBrowserApp/Height :: type-union(<integer>, <machine-word>), 
        name: "Height", disp-id: 209;

  /* Gets the short (UI-friendly) name of the URL/file currently viewed. */
  constant property IWebBrowserApp/LocationName :: <string>, name: 
        "LocationName", disp-id: 210;

  /* Gets the full URL/path currently viewed. */
  constant property IWebBrowserApp/LocationURL :: <string>, name: 
        "LocationURL", disp-id: 211;

  /* Query to see if something is still in progress. */
  constant property IWebBrowserApp/Busy :: <boolean>, name: "Busy", 
        disp-id: 212;

  /* Exits application and closes the open document. */
  function IWebBrowserApp/Quit () => (), name: "Quit", disp-id: 300;

  /* Converts client sizes into window sizes. */
  function IWebBrowserApp/ClientToWindow (arg-pcx :: 
        inout-ref(<C-signed-long>), arg-pcy :: inout-ref(<C-signed-long>)) 
        => (), name: "ClientToWindow", disp-id: 301;

  /* Associates vtValue with the name szProperty in the context of the 
        object. */
  function IWebBrowserApp/PutProperty (arg-Property :: <string>, 
        arg-vtValue :: <object>) => (), name: "PutProperty", disp-id: 302;

  /* Retrieve the Associated value for the property vtValue in the context 
        of the object. */
  function IWebBrowserApp/GetProperty (arg-Property :: <string>) => 
        (arg-result :: <object>), name: "GetProperty", disp-id: 303;

  /* Returns name of the application. */
  constant property IWebBrowserApp/Name :: <string>, name: "Name", disp-id: 
        0;

  /* Returns the HWND of the current IE window. */
  constant property IWebBrowserApp/HWND :: type-union(<integer>, 
        <machine-word>), name: "HWND", disp-id: -515;

  /* Returns file specification of the application, including path. */
  constant property IWebBrowserApp/FullName :: <string>, name: "FullName", 
        disp-id: 400;

  /* Returns the path to the application. */
  constant property IWebBrowserApp/Path :: <string>, name: "Path", disp-id: 
        401;

  /* Determines whether the application is visible or hidden. */
  property IWebBrowserApp/Visible :: <boolean>, name: "Visible", disp-id: 
        402;

  /* Turn on or off the statusbar. */
  property IWebBrowserApp/StatusBar :: <boolean>, name: "StatusBar", 
        disp-id: 403;

  /* Text of Status window. */
  property IWebBrowserApp/StatusText :: <string>, name: "StatusText", 
        disp-id: 404;

  /* Controls which toolbar is shown. */
  property IWebBrowserApp/ToolBar :: type-union(<integer>, <machine-word>), 
        name: "ToolBar", disp-id: 405;

  /* Controls whether menubar is shown. */
  property IWebBrowserApp/MenuBar :: <boolean>, name: "MenuBar", disp-id: 
        406;

  /* Maximizes window and turns off statusbar, toolbar, menubar, and 
        titlebar. */
  property IWebBrowserApp/FullScreen :: <boolean>, name: "FullScreen", 
        disp-id: 407;
end dispatch-client <IWebBrowserApp>;


/* hidden? Dispatch interface: IWebBrowser2 version 0.0
 * GUID: {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
 * Description: Web Browser Interface for IE4.
 */
define dispatch-client <IWebBrowser2>
  uuid "{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}";

  /* Navigates to the previous item in the history list. */
  function IWebBrowser2/GoBack () => (), name: "GoBack", disp-id: 100;

  /* Navigates to the next item in the history list. */
  function IWebBrowser2/GoForward () => (), name: "GoForward", disp-id: 
        101;

  /* Go home/start page. */
  function IWebBrowser2/GoHome () => (), name: "GoHome", disp-id: 102;

  /* Go Search Page. */
  function IWebBrowser2/GoSearch () => (), name: "GoSearch", disp-id: 103;

  /* Navigates to a URL or file. */
  function IWebBrowser2/Navigate (arg-URL :: <string>, /*optional*/ 
        arg-Flags :: <VARIANT*>, /*optional*/ arg-TargetFrameName :: 
        <VARIANT*>, /*optional*/ arg-PostData :: <VARIANT*>, /*optional*/ 
        arg-Headers :: <VARIANT*>) => (), name: "Navigate", disp-id: 104;

  /* Refresh the currently viewed page. */
  function IWebBrowser2/Refresh () => (), name: "Refresh", disp-id: -550;

  /* Refresh the currently viewed page. */
  function IWebBrowser2/Refresh2 (/*optional*/ arg-Level :: <VARIANT*>) => 
        (), name: "Refresh2", disp-id: 105;

  /* Stops opening a file. */
  function IWebBrowser2/Stop () => (), name: "Stop", disp-id: 106;

  /* Returns the application automation object if accessible, this 
        automation object otherwise.. */
  constant property IWebBrowser2/Application :: <LPDISPATCH>, name: 
        "Application", disp-id: 200;

  /* Returns the automation object of the container/parent if one exists or 
        this automation object. */
  constant property IWebBrowser2/Parent :: <LPDISPATCH>, name: "Parent", 
        disp-id: 201;

  /* Returns the container/parent automation object, if any. */
  constant property IWebBrowser2/Container :: <LPDISPATCH>, name: 
        "Container", disp-id: 202;

  /* Returns the active Document automation object, if any. */
  constant property IWebBrowser2/Document :: <LPDISPATCH>, name: 
        "Document", disp-id: 203;

  /* Returns True if this is the top level object. */
  constant property IWebBrowser2/TopLevelContainer :: <boolean>, name: 
        "TopLevelContainer", disp-id: 204;

  /* Returns the type of the contained document object. */
  constant property IWebBrowser2/Type :: <string>, name: "Type", disp-id: 
        205;

  /* The horizontal position (pixels) of the frame window relative to the 
        screen/container. */
  property IWebBrowser2/Left :: type-union(<integer>, <machine-word>), 
        name: "Left", disp-id: 206;

  /* The vertical position (pixels) of the frame window relative to the 
        screen/container. */
  property IWebBrowser2/Top :: type-union(<integer>, <machine-word>), name: 
        "Top", disp-id: 207;

  /* The horizontal dimension (pixels) of the frame window/object. */
  property IWebBrowser2/Width :: type-union(<integer>, <machine-word>), 
        name: "Width", disp-id: 208;

  /* The vertical dimension (pixels) of the frame window/object. */
  property IWebBrowser2/Height :: type-union(<integer>, <machine-word>), 
        name: "Height", disp-id: 209;

  /* Gets the short (UI-friendly) name of the URL/file currently viewed. */
  constant property IWebBrowser2/LocationName :: <string>, name: 
        "LocationName", disp-id: 210;

  /* Gets the full URL/path currently viewed. */
  constant property IWebBrowser2/LocationURL :: <string>, name: 
        "LocationURL", disp-id: 211;

  /* Query to see if something is still in progress. */
  constant property IWebBrowser2/Busy :: <boolean>, name: "Busy", disp-id: 
        212;

  /* Exits application and closes the open document. */
  function IWebBrowser2/Quit () => (), name: "Quit", disp-id: 300;

  /* Converts client sizes into window sizes. */
  function IWebBrowser2/ClientToWindow (arg-pcx :: 
        inout-ref(<C-signed-long>), arg-pcy :: inout-ref(<C-signed-long>)) 
        => (), name: "ClientToWindow", disp-id: 301;

  /* Associates vtValue with the name szProperty in the context of the 
        object. */
  function IWebBrowser2/PutProperty (arg-Property :: <string>, arg-vtValue 
        :: <object>) => (), name: "PutProperty", disp-id: 302;

  /* Retrieve the Associated value for the property vtValue in the context 
        of the object. */
  function IWebBrowser2/GetProperty (arg-Property :: <string>) => 
        (arg-result :: <object>), name: "GetProperty", disp-id: 303;

  /* Returns name of the application. */
  constant property IWebBrowser2/Name :: <string>, name: "Name", disp-id: 
        0;

  /* Returns the HWND of the current IE window. */
  constant property IWebBrowser2/HWND :: type-union(<integer>, 
        <machine-word>), name: "HWND", disp-id: -515;

  /* Returns file specification of the application, including path. */
  constant property IWebBrowser2/FullName :: <string>, name: "FullName", 
        disp-id: 400;

  /* Returns the path to the application. */
  constant property IWebBrowser2/Path :: <string>, name: "Path", disp-id: 
        401;

  /* Determines whether the application is visible or hidden. */
  property IWebBrowser2/Visible :: <boolean>, name: "Visible", disp-id: 
        402;

  /* Turn on or off the statusbar. */
  property IWebBrowser2/StatusBar :: <boolean>, name: "StatusBar", disp-id: 
        403;

  /* Text of Status window. */
  property IWebBrowser2/StatusText :: <string>, name: "StatusText", 
        disp-id: 404;

  /* Controls which toolbar is shown. */
  property IWebBrowser2/ToolBar :: type-union(<integer>, <machine-word>), 
        name: "ToolBar", disp-id: 405;

  /* Controls whether menubar is shown. */
  property IWebBrowser2/MenuBar :: <boolean>, name: "MenuBar", disp-id: 
        406;

  /* Maximizes window and turns off statusbar, toolbar, menubar, and 
        titlebar. */
  property IWebBrowser2/FullScreen :: <boolean>, name: "FullScreen", 
        disp-id: 407;

  /* Navigates to a URL or file or pidl. */
  function IWebBrowser2/Navigate2 (arg-URL :: <VARIANT*>, /*optional*/ 
        arg-Flags :: <VARIANT*>, /*optional*/ arg-TargetFrameName :: 
        <VARIANT*>, /*optional*/ arg-PostData :: <VARIANT*>, /*optional*/ 
        arg-Headers :: <VARIANT*>) => (), name: "Navigate2", disp-id: 500;

  /* IOleCommandTarget::QueryStatus */
  function IWebBrowser2/QueryStatusWB (arg-cmdID :: <OLECMDID>) => 
        (arg-result :: <OLECMDF>), name: "QueryStatusWB", disp-id: 501;

  /* IOleCommandTarget::Exec */
  function IWebBrowser2/ExecWB (arg-cmdID :: <OLECMDID>, arg-cmdexecopt :: 
        <OLECMDEXECOPT>, /*optional*/ arg-pvaIn :: <VARIANT*>, /*optional*/ 
        arg-pvaOut :: inout-ref(<VARIANT>)) => (), name: "ExecWB", disp-id: 
        502;

  /* Set BrowserBar to Clsid */
  function IWebBrowser2/ShowBrowserBar (arg-pvaClsid :: <VARIANT*>, 
        /*optional*/ arg-pvarShow :: <VARIANT*>, /*optional*/ arg-pvarSize 
        :: <VARIANT*>) => (), name: "ShowBrowserBar", disp-id: 503;

  constant property IWebBrowser2/ReadyState :: <tagREADYSTATE>, name: 
        "ReadyState", disp-id: -525;

  /* Controls if the frame is offline (read from cache) */
  property IWebBrowser2/Offline :: <boolean>, name: "Offline", disp-id: 
        550;

  /* Controls if any dialog boxes can be shown */
  property IWebBrowser2/Silent :: <boolean>, name: "Silent", disp-id: 551;

  /* Registers OC as a top-level browser (for target name resolution) */
  property IWebBrowser2/RegisterAsBrowser :: <boolean>, name: 
        "RegisterAsBrowser", disp-id: 552;

  /* Registers OC as a drop target for navigation */
  property IWebBrowser2/RegisterAsDropTarget :: <boolean>, name: 
        "RegisterAsDropTarget", disp-id: 553;

  /* Controls if the browser is in theater mode */
  property IWebBrowser2/TheaterMode :: <boolean>, name: "TheaterMode", 
        disp-id: 554;

  /* Controls whether address bar is shown */
  property IWebBrowser2/AddressBar :: <boolean>, name: "AddressBar", 
        disp-id: 555;

  /* Controls whether the window is resizable */
  property IWebBrowser2/Resizable :: <boolean>, name: "Resizable", disp-id: 
        556;
end dispatch-client <IWebBrowser2>;


/* Enumeration: OLECMDID
 */
define constant <OLECMDID> = type-union(<integer>, <machine-word>);
define constant $OLECMDID-OPEN = 1;
define constant $OLECMDID-NEW = 2;
define constant $OLECMDID-SAVE = 3;
define constant $OLECMDID-SAVEAS = 4;
define constant $OLECMDID-SAVECOPYAS = 5;
define constant $OLECMDID-PRINT = 6;
define constant $OLECMDID-PRINTPREVIEW = 7;
define constant $OLECMDID-PAGESETUP = 8;
define constant $OLECMDID-SPELL = 9;
define constant $OLECMDID-PROPERTIES = 10;
define constant $OLECMDID-CUT = 11;
define constant $OLECMDID-COPY = 12;
define constant $OLECMDID-PASTE = 13;
define constant $OLECMDID-PASTESPECIAL = 14;
define constant $OLECMDID-UNDO = 15;
define constant $OLECMDID-REDO = 16;
define constant $OLECMDID-SELECTALL = 17;
define constant $OLECMDID-CLEARSELECTION = 18;
define constant $OLECMDID-ZOOM = 19;
define constant $OLECMDID-GETZOOMRANGE = 20;
define constant $OLECMDID-UPDATECOMMANDS = 21;
define constant $OLECMDID-REFRESH = 22;
define constant $OLECMDID-STOP = 23;
define constant $OLECMDID-HIDETOOLBARS = 24;
define constant $OLECMDID-SETPROGRESSMAX = 25;
define constant $OLECMDID-SETPROGRESSPOS = 26;
define constant $OLECMDID-SETPROGRESSTEXT = 27;
define constant $OLECMDID-SETTITLE = 28;
define constant $OLECMDID-SETDOWNLOADSTATE = 29;
define constant $OLECMDID-STOPDOWNLOAD = 30;
define constant $OLECMDID-ONTOOLBARACTIVATED = 31;
define constant $OLECMDID-FIND = 32;
define constant $OLECMDID-DELETE = 33;
define constant $OLECMDID-HTTPEQUIV = 34;
define constant $OLECMDID-HTTPEQUIV-DONE = 35;
define constant $OLECMDID-ENABLE-INTERACTION = 36;
define constant $OLECMDID-ONUNLOAD = 37;
define constant $OLECMDID-PROPERTYBAG2 = 38;
define constant $OLECMDID-PREREFRESH = 39;
define constant $OLECMDID-SHOWSCRIPTERROR = 40;
define constant $OLECMDID-SHOWMESSAGE = 41;
define constant $OLECMDID-SHOWFIND = 42;
define constant $OLECMDID-SHOWPAGESETUP = 43;
define constant $OLECMDID-SHOWPRINT = 44;
define constant $OLECMDID-CLOSE = 45;
define constant $OLECMDID-ALLOWUILESSSAVEAS = 46;
define constant $OLECMDID-DONTDOWNLOADCSS = 47;
define constant $OLECMDID-UPDATEPAGESTATUS = 48;


/* Enumeration: OLECMDF
 */
define constant <OLECMDF> = type-union(<integer>, <machine-word>);
define constant $OLECMDF-SUPPORTED = 1;
define constant $OLECMDF-ENABLED = 2;
define constant $OLECMDF-LATCHED = 4;
define constant $OLECMDF-NINCHED = 8;
define constant $OLECMDF-INVISIBLE = 16;
define constant $OLECMDF-DEFHIDEONCTXTMENU = 32;


/* Enumeration: OLECMDEXECOPT
 */
define constant <OLECMDEXECOPT> = type-union(<integer>, <machine-word>);
define constant $OLECMDEXECOPT-DODEFAULT = 0;
define constant $OLECMDEXECOPT-PROMPTUSER = 1;
define constant $OLECMDEXECOPT-DONTPROMPTUSER = 2;
define constant $OLECMDEXECOPT-SHOWHELP = 3;


/* Enumeration: tagREADYSTATE
 */
define constant <tagREADYSTATE> = type-union(<integer>, <machine-word>);
define constant $READYSTATE-UNINITIALIZED = 0;
define constant $READYSTATE-LOADING = 1;
define constant $READYSTATE-LOADED = 2;
define constant $READYSTATE-INTERACTIVE = 3;
define constant $READYSTATE-COMPLETE = 4;


/* Enumeration: SecureLockIconConstants
 * Description: Constants for WebBrowser security icon notification
 */
define constant <SecureLockIconConstants> = type-union(<integer>, 
        <machine-word>);
define constant $secureLockIconUnsecure = 0;
define constant $secureLockIconMixed = 1;
define constant $secureLockIconSecureUnknownBits = 2;
define constant $secureLockIconSecure40Bit = 3;
define constant $secureLockIconSecure56Bit = 4;
define constant $secureLockIconSecureFortezza = 5;
define constant $secureLockIconSecure128Bit = 6;


/* hidden? Dispatch interface: DWebBrowserEvents2 version 0.0
 * GUID: {34A715A0-6587-11D0-924A-0020AFC7AC4D}
 * Description: Web Browser Control events interface
 */
define dispatch-client <DWebBrowserEvents2>
  uuid "{34A715A0-6587-11D0-924A-0020AFC7AC4D}";

  /* Statusbar text changed. */
  function DWebBrowserEvents2/StatusTextChange (arg-Text :: <string>) => 
        (), name: "StatusTextChange", disp-id: 102;

  /* Fired when download progress is updated. */
  function DWebBrowserEvents2/ProgressChange (arg-Progress :: 
        type-union(<integer>, <machine-word>), arg-ProgressMax :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "ProgressChange", disp-id: 108;

  /* The enabled state of a command changed. */
  function DWebBrowserEvents2/CommandStateChange (arg-Command :: 
        type-union(<integer>, <machine-word>), arg-Enable :: <boolean>) => 
        (), name: "CommandStateChange", disp-id: 105;

  /* Download of a page started. */
  function DWebBrowserEvents2/DownloadBegin () => (), name: 
        "DownloadBegin", disp-id: 106;

  /* Download of page complete. */
  function DWebBrowserEvents2/DownloadComplete () => (), name: 
        "DownloadComplete", disp-id: 104;

  /* Document title changed. */
  function DWebBrowserEvents2/TitleChange (arg-Text :: <string>) => (), 
        name: "TitleChange", disp-id: 113;

  /* Fired when the PutProperty method has been called. */
  function DWebBrowserEvents2/PropertyChange (arg-szProperty :: <string>) 
        => (), name: "PropertyChange", disp-id: 112;

  /* Fired before navigate occurs in the given WebBrowser (window or 
        frameset element). The processing of this navigation may be 
        modified. */
  function DWebBrowserEvents2/BeforeNavigate2 (arg-pDisp :: <LPDISPATCH>, 
        arg-URL :: <VARIANT*>, arg-Flags :: <VARIANT*>, arg-TargetFrameName 
        :: <VARIANT*>, arg-PostData :: <VARIANT*>, arg-Headers :: 
        <VARIANT*>, arg-Cancel :: inout-ref(<VARIANT-BOOL>)) => (), name: 
        "BeforeNavigate2", disp-id: 250;

  /* A new, hidden, non-navigated WebBrowser window is needed. */
  function DWebBrowserEvents2/NewWindow2 (arg-ppDisp :: 
        inout-ref(<LPDISPATCH>), arg-Cancel :: inout-ref(<VARIANT-BOOL>)) 
        => (), name: "NewWindow2", disp-id: 251;

  /* Fired when the document being navigated to becomes visible and enters 
        the navigation stack. */
  function DWebBrowserEvents2/NavigateComplete2 (arg-pDisp :: <LPDISPATCH>, 
        arg-URL :: <VARIANT*>) => (), name: "NavigateComplete2", disp-id: 
        252;

  /* Fired when the document being navigated to reaches 
        ReadyState_Complete. */
  function DWebBrowserEvents2/DocumentComplete (arg-pDisp :: <LPDISPATCH>, 
        arg-URL :: <VARIANT*>) => (), name: "DocumentComplete", disp-id: 
        259;

  /* Fired when application is quiting. */
  function DWebBrowserEvents2/OnQuit () => (), name: "OnQuit", disp-id: 
        253;

  /* Fired when the window should be shown/hidden */
  function DWebBrowserEvents2/OnVisible (arg-Visible :: <boolean>) => (), 
        name: "OnVisible", disp-id: 254;

  /* Fired when the toolbar  should be shown/hidden */
  function DWebBrowserEvents2/OnToolBar (arg-ToolBar :: <boolean>) => (), 
        name: "OnToolBar", disp-id: 255;

  /* Fired when the menubar should be shown/hidden */
  function DWebBrowserEvents2/OnMenuBar (arg-MenuBar :: <boolean>) => (), 
        name: "OnMenuBar", disp-id: 256;

  /* Fired when the statusbar should be shown/hidden */
  function DWebBrowserEvents2/OnStatusBar (arg-StatusBar :: <boolean>) => 
        (), name: "OnStatusBar", disp-id: 257;

  /* Fired when fullscreen mode should be on/off */
  function DWebBrowserEvents2/OnFullScreen (arg-FullScreen :: <boolean>) => 
        (), name: "OnFullScreen", disp-id: 258;

  /* Fired when theater mode should be on/off */
  function DWebBrowserEvents2/OnTheaterMode (arg-TheaterMode :: <boolean>) 
        => (), name: "OnTheaterMode", disp-id: 260;

  /* Fired when the host window should allow/disallow resizing */
  function DWebBrowserEvents2/WindowSetResizable (arg-Resizable :: 
        <boolean>) => (), name: "WindowSetResizable", disp-id: 262;

  /* Fired when the host window should change its Left coordinate */
  function DWebBrowserEvents2/WindowSetLeft (arg-Left :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "WindowSetLeft", disp-id: 264;

  /* Fired when the host window should change its Top coordinate */
  function DWebBrowserEvents2/WindowSetTop (arg-Top :: 
        type-union(<integer>, <machine-word>)) => (), name: "WindowSetTop", 
        disp-id: 265;

  /* Fired when the host window should change its width */
  function DWebBrowserEvents2/WindowSetWidth (arg-Width :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "WindowSetWidth", disp-id: 266;

  /* Fired when the host window should change its height */
  function DWebBrowserEvents2/WindowSetHeight (arg-Height :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "WindowSetHeight", disp-id: 267;

  /* Fired when the WebBrowser is about to be closed by script */
  function DWebBrowserEvents2/WindowClosing (arg-IsChildWindow :: 
        <boolean>, arg-Cancel :: inout-ref(<VARIANT-BOOL>)) => (), name: 
        "WindowClosing", disp-id: 263;

  /* Fired to request client sizes be converted to host window sizes */
  function DWebBrowserEvents2/ClientToHostWindow (arg-CX :: 
        inout-ref(<C-signed-long>), arg-CY :: inout-ref(<C-signed-long>)) 
        => (), name: "ClientToHostWindow", disp-id: 268;

  /* Fired to indicate the security level of the current web page contents 
        */
  function DWebBrowserEvents2/SetSecureLockIcon (arg-SecureLockIcon :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "SetSecureLockIcon", disp-id: 269;

  /* Fired to indicate the File Download dialog is opening */
  function DWebBrowserEvents2/FileDownload (arg-Cancel :: 
        inout-ref(<VARIANT-BOOL>)) => (), name: "FileDownload", disp-id: 
        270;
end dispatch-client <DWebBrowserEvents2>;


/* COM class: WebBrowser_V1 version 0.0
 * GUID: {EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}
 * Description: WebBrowser Control
 */
define constant $WebBrowser-V1-class-id = as(<REFCLSID>, 
        "{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}");

define function make-WebBrowser-V1 () => (default-interface :: 
        <IWebBrowser>, interface-2 :: <IWebBrowser2>)
  /* Translation error: source interface WebBrowser_V1 not supported. */
  /* Translation error: source interface WebBrowser_V1 not supported. */
  let default-interface = make(<IWebBrowser>, class-id: 
        $WebBrowser-V1-class-id);
  values(default-interface,
         make(<IWebBrowser2>, disp-interface: default-interface))
end function make-WebBrowser-V1;


/* COM class: WebBrowser version 0.0
 * GUID: {8856F961-340A-11D0-A96B-00C04FD705A2}
 * Description: WebBrowser Control
 */
define constant $WebBrowser-class-id = as(<REFCLSID>, 
        "{8856F961-340A-11D0-A96B-00C04FD705A2}");

define function make-WebBrowser () => (default-interface :: <IWebBrowser2>, 
        interface-2 :: <IWebBrowser>)
  /* Translation error: source interface WebBrowser not supported. */
  /* Translation error: source interface WebBrowser not supported. */
  let default-interface = make(<IWebBrowser2>, class-id: 
        $WebBrowser-class-id);
  values(default-interface,
         make(<IWebBrowser>, disp-interface: default-interface))
end function make-WebBrowser;


/* COM class: InternetExplorer version 0.0
 * GUID: {0002DF01-0000-0000-C000-000000000046}
 * Description: Internet Explorer Application.
 */
define constant $InternetExplorer-class-id = as(<REFCLSID>, 
        "{0002DF01-0000-0000-C000-000000000046}");

define function make-InternetExplorer () => (default-interface :: 
        <IWebBrowser2>, interface-2 :: <IWebBrowserApp>)
  /* Translation error: source interface InternetExplorer not supported. */
  /* Translation error: source interface InternetExplorer not supported. */
  let default-interface = make(<IWebBrowser2>, class-id: 
        $InternetExplorer-class-id);
  values(default-interface,
         make(<IWebBrowserApp>, disp-interface: default-interface))
end function make-InternetExplorer;


/* hidden? COM class: ShellBrowserWindow version 0.0
 * GUID: {C08AFD90-F2A1-11D1-8455-00A0C91F3880}
 * Description: Shell Browser Window.
 */
define constant $ShellBrowserWindow-class-id = as(<REFCLSID>, 
        "{C08AFD90-F2A1-11D1-8455-00A0C91F3880}");

define function make-ShellBrowserWindow () => (default-interface :: 
        <IWebBrowser2>, interface-2 :: <IWebBrowserApp>)
  /* Translation error: source interface ShellBrowserWindow not supported. 
        */
  /* Translation error: source interface ShellBrowserWindow not supported. 
        */
  let default-interface = make(<IWebBrowser2>, class-id: 
        $ShellBrowserWindow-class-id);
  values(default-interface,
         make(<IWebBrowserApp>, disp-interface: default-interface))
end function make-ShellBrowserWindow;


/* Enumeration: ShellWindowTypeConstants
 * Description: Constants for ShellWindows registration
 */
define constant <ShellWindowTypeConstants> = type-union(<integer>, 
        <machine-word>);
define constant $SWC-EXPLORER = 0;
define constant $SWC-BROWSER = 1;
define constant $SWC-3RDPARTY = 2;
define constant $SWC-CALLBACK = 4;


/* hidden? Enumeration: ShellWindowFindWindowOptions
 * Description: Options for ShellWindows FindWindow
 */
define constant <ShellWindowFindWindowOptions> = type-union(<integer>, 
        <machine-word>);
define constant $SWFO-NEEDDISPATCH = 1;
define constant $SWFO-INCLUDEPENDING = 2;
define constant $SWFO-COOKIEPASSED = 4;


/* Dispatch interface: DShellWindowsEvents version 0.0
 * GUID: {FE4106E0-399A-11D0-A48C-00A0C90A8F39}
 * Description: Event interface for IShellWindows
 */
define dispatch-client <DShellWindowsEvents>
  uuid "{FE4106E0-399A-11D0-A48C-00A0C90A8F39}";

  /* A new window was registered. */
  function DShellWindowsEvents/WindowRegistered (arg-lCookie :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "WindowRegistered", disp-id: 200;

  /* A new window was revoked. */
  function DShellWindowsEvents/WindowRevoked (arg-lCookie :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "WindowRevoked", disp-id: 201;
end dispatch-client <DShellWindowsEvents>;


/* Dispatch interface: IShellWindows version 0.0
 * GUID: {85CB6900-4D95-11CF-960C-0080C7F4EE85}
 * Description: Definition of interface IShellWindows
 */
define dispatch-client <IShellWindows>
  uuid "{85CB6900-4D95-11CF-960C-0080C7F4EE85}";

  /* Get count of open Shell windows */
  size constant property IShellWindows/Count :: type-union(<integer>, 
        <machine-word>), name: "Count", disp-id: as(<machine-word>, 
        #x60020000);

  /* Return the shell window for the given index */
  function IShellWindows/Item (/*optional*/ arg-index :: <object>) => 
        (arg-result :: <LPDISPATCH>), name: "Item", disp-id: 0;

  /* Enumerates the figures */
  function IShellWindows/_NewEnum () => (arg-result :: <LPUNKNOWN>), name: 
        "_NewEnum", disp-id: -4;

  /* Register a window with the list */
  function IShellWindows/Register (arg-pid :: <LPDISPATCH>, arg-HWND :: 
        type-union(<integer>, <machine-word>), arg-swClass :: 
        type-union(<integer>, <machine-word>), arg-plCookie :: 
        out-ref(<C-signed-long>)) => (), name: "Register", disp-id: 
        as(<machine-word>, #x60020003);

  /* Register a pending open with the list */
  function IShellWindows/RegisterPending (arg-lThreadId :: 
        type-union(<integer>, <machine-word>), arg-pvarloc :: <VARIANT*>, 
        arg-pvarlocRoot :: <VARIANT*>, arg-swClass :: type-union(<integer>, 
        <machine-word>), arg-plCookie :: out-ref(<C-signed-long>)) => (), 
        name: "RegisterPending", disp-id: as(<machine-word>, #x60020004);

  /* Remove a window from the list */
  function IShellWindows/Revoke (arg-lCookie :: type-union(<integer>, 
        <machine-word>)) => (), name: "Revoke", disp-id: as(<machine-word>, 
        #x60020005);

  /* Notifies the new location */
  function IShellWindows/OnNavigate (arg-lCookie :: type-union(<integer>, 
        <machine-word>), arg-pvarloc :: <VARIANT*>) => (), name: 
        "OnNavigate", disp-id: as(<machine-word>, #x60020006);

  /* Notifies the activation */
  function IShellWindows/OnActivated (arg-lCookie :: type-union(<integer>, 
        <machine-word>), arg-fActive :: <boolean>) => (), name: 
        "OnActivated", disp-id: as(<machine-word>, #x60020007);

  /* Find the window based on the location */
  function IShellWindows/FindWindowSW (arg-pvarloc :: <VARIANT*>, 
        arg-pvarlocRoot :: <VARIANT*>, arg-swClass :: type-union(<integer>, 
        <machine-word>), arg-pHWND :: out-ref(<C-signed-long>), 
        arg-swfwOptions :: type-union(<integer>, <machine-word>)) => 
        (arg-result :: <LPDISPATCH>), name: "FindWindowSW", disp-id: 
        as(<machine-word>, #x60020008);

  /* Notifies on creation and frame name set */
  function IShellWindows/OnCreated (arg-lCookie :: type-union(<integer>, 
        <machine-word>), arg-punk :: <LPUNKNOWN>) => (), name: "OnCreated", 
        disp-id: as(<machine-word>, #x60020009);

  /* Used by IExplore to register different processes */
  function IShellWindows/ProcessAttachDetach (arg-fAttach :: <boolean>) => 
        (), name: "ProcessAttachDetach", disp-id: as(<machine-word>, 
        #x6002000A);
end dispatch-client <IShellWindows>;


/* COM class: ShellWindows version 0.0
 * GUID: {9BA05972-F6A8-11CF-A442-00A0C90A8F39}
 * Description: ShellDispatch Load in Shell Context
 */
define constant $ShellWindows-class-id = as(<REFCLSID>, 
        "{9BA05972-F6A8-11CF-A442-00A0C90A8F39}");

define function make-ShellWindows () => (default-interface :: 
        <IShellWindows>)
  /* Translation error: source interface ShellWindows not supported. */
  let default-interface = make(<IShellWindows>, class-id: 
        $ShellWindows-class-id);
  values(default-interface)
end function make-ShellWindows;


/* Dispatch interface: IShellUIHelper version 0.0
 * GUID: {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
 * Description: Shell UI Helper Control Interface
 */
define dispatch-client <IShellUIHelper>
  uuid "{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}";

  function IShellUIHelper/ResetFirstBootMode () => (), name: 
        "ResetFirstBootMode", disp-id: 1;

  function IShellUIHelper/ResetSafeMode () => (), name: "ResetSafeMode", 
        disp-id: 2;

  function IShellUIHelper/RefreshOfflineDesktop () => (), name: 
        "RefreshOfflineDesktop", disp-id: 3;

  function IShellUIHelper/AddFavorite (arg-URL :: <string>, /*optional*/ 
        arg-Title :: <VARIANT*>) => (), name: "AddFavorite", disp-id: 4;

  function IShellUIHelper/AddChannel (arg-URL :: <string>) => (), name: 
        "AddChannel", disp-id: 5;

  function IShellUIHelper/AddDesktopComponent (arg-URL :: <string>, 
        arg-Type :: <string>, /*optional*/ arg-Left :: <VARIANT*>, 
        /*optional*/ arg-Top :: <VARIANT*>, /*optional*/ arg-Width :: 
        <VARIANT*>, /*optional*/ arg-Height :: <VARIANT*>) => (), name: 
        "AddDesktopComponent", disp-id: 6;

  function IShellUIHelper/IsSubscribed (arg-URL :: <string>) => (arg-result 
        :: <boolean>), name: "IsSubscribed", disp-id: 7;

  function IShellUIHelper/NavigateAndFind (arg-URL :: <string>, 
        arg-strQuery :: <string>, arg-varTargetFrame :: <VARIANT*>) => (), 
        name: "NavigateAndFind", disp-id: 8;

  function IShellUIHelper/ImportExportFavorites (arg-fImport :: <boolean>, 
        arg-strImpExpPath :: <string>) => (), name: 
        "ImportExportFavorites", disp-id: 9;

  function IShellUIHelper/AutoCompleteSaveForm (/*optional*/ arg-Form :: 
        <VARIANT*>) => (), name: "AutoCompleteSaveForm", disp-id: 10;

  function IShellUIHelper/AutoScan (arg-strSearch :: <string>, 
        arg-strFailureUrl :: <string>, /*optional*/ arg-pvarTargetFrame :: 
        <VARIANT*>) => (), name: "AutoScan", disp-id: 11;

  function IShellUIHelper/AutoCompleteAttach (/*optional*/ arg-Reserved :: 
        <VARIANT*>) => (), name: "AutoCompleteAttach", disp-id: 12;

  function IShellUIHelper/ShowBrowserUI (arg-bstrName :: <string>, 
        arg-pvarIn :: <VARIANT*>) => (arg-result :: <object>), name: 
        "ShowBrowserUI", disp-id: 13;
end dispatch-client <IShellUIHelper>;


/* COM class: ShellUIHelper version 0.0
 * GUID: {64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}
 */
define constant $ShellUIHelper-class-id = as(<REFCLSID>, 
        "{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}");

define function make-ShellUIHelper () => (default-interface :: 
        <IShellUIHelper>)
  let default-interface = make(<IShellUIHelper>, class-id: 
        $ShellUIHelper-class-id);
  values(default-interface)
end function make-ShellUIHelper;


/* Dispatch interface: DShellNameSpaceEvents version 0.0
 * GUID: {55136806-B2DE-11D1-B9F2-00A0C98BC547}
 */
define dispatch-client <DShellNameSpaceEvents>
  uuid "{55136806-B2DE-11D1-B9F2-00A0C98BC547}";

  function DShellNameSpaceEvents/FavoritesSelectionChange (arg-cItems :: 
        type-union(<integer>, <machine-word>), arg-hItem :: 
        type-union(<integer>, <machine-word>), arg-strName :: <string>, 
        arg-strUrl :: <string>, arg-cVisits :: type-union(<integer>, 
        <machine-word>), arg-strDate :: <string>, arg-fAvailableOffline :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "FavoritesSelectionChange", disp-id: 1;

  function DShellNameSpaceEvents/SelectionChange () => (), name: 
        "SelectionChange", disp-id: 2;

  function DShellNameSpaceEvents/DoubleClick () => (), name: "DoubleClick", 
        disp-id: 3;

  function DShellNameSpaceEvents/Initialized () => (), name: "Initialized", 
        disp-id: 4;
end dispatch-client <DShellNameSpaceEvents>;


/* hidden? Dispatch interface: IShellFavoritesNameSpace version 0.0
 * GUID: {55136804-B2DE-11D1-B9F2-00A0C98BC547}
 * Description: IShellFavoritesNameSpace Interface
 */
define dispatch-client <IShellFavoritesNameSpace>
  uuid "{55136804-B2DE-11D1-B9F2-00A0C98BC547}";

  /* method MoveSelectionUp */
  function IShellFavoritesNameSpace/MoveSelectionUp () => (), name: 
        "MoveSelectionUp", disp-id: 1;

  /* method MoveSelectionDown */
  function IShellFavoritesNameSpace/MoveSelectionDown () => (), name: 
        "MoveSelectionDown", disp-id: 2;

  /* method ResetSort */
  function IShellFavoritesNameSpace/ResetSort () => (), name: "ResetSort", 
        disp-id: 3;

  /* method NewFolder */
  function IShellFavoritesNameSpace/NewFolder () => (), name: "NewFolder", 
        disp-id: 4;

  /* method Synchronize */
  function IShellFavoritesNameSpace/Synchronize () => (), name: 
        "Synchronize", disp-id: 5;

  /* method Import */
  function IShellFavoritesNameSpace/Import () => (), name: "Import", 
        disp-id: 6;

  /* method Export */
  function IShellFavoritesNameSpace/Export () => (), name: "Export", 
        disp-id: 7;

  /* method InvokeContextMenuCommand */
  function IShellFavoritesNameSpace/InvokeContextMenuCommand 
        (arg-strCommand :: <string>) => (), name: 
        "InvokeContextMenuCommand", disp-id: 8;

  /* method MoveSelectionTo */
  function IShellFavoritesNameSpace/MoveSelectionTo () => (), name: 
        "MoveSelectionTo", disp-id: 9;

  /* Query to see if subscriptions are enabled */
  constant property IShellFavoritesNameSpace/SubscriptionsEnabled :: 
        <boolean>, name: "SubscriptionsEnabled", disp-id: 10;

  /* method CreateSubscriptionForSelection */
  function IShellFavoritesNameSpace/CreateSubscriptionForSelection () => 
        (arg-result :: <boolean>), name: "CreateSubscriptionForSelection", 
        disp-id: 11;

  /* method DeleteSubscriptionForSelection */
  function IShellFavoritesNameSpace/DeleteSubscriptionForSelection () => 
        (arg-result :: <boolean>), name: "DeleteSubscriptionForSelection", 
        disp-id: 12;

  /* old, use put_Root() instead */
  function IShellFavoritesNameSpace/SetRoot (arg-bstrFullPath :: <string>) 
        => (), name: "SetRoot", disp-id: 13;
end dispatch-client <IShellFavoritesNameSpace>;


/* hidden? Dispatch interface: IShellNameSpace version 0.0
 * GUID: {E572D3C9-37BE-4AE2-825D-D521763E3108}
 * Description: IShellNameSpace Interface
 */
define dispatch-client <IShellNameSpace>
  uuid "{E572D3C9-37BE-4AE2-825D-D521763E3108}";

  /* method MoveSelectionUp */
  function IShellNameSpace/MoveSelectionUp () => (), name: 
        "MoveSelectionUp", disp-id: 1;

  /* method MoveSelectionDown */
  function IShellNameSpace/MoveSelectionDown () => (), name: 
        "MoveSelectionDown", disp-id: 2;

  /* method ResetSort */
  function IShellNameSpace/ResetSort () => (), name: "ResetSort", disp-id: 
        3;

  /* method NewFolder */
  function IShellNameSpace/NewFolder () => (), name: "NewFolder", disp-id: 
        4;

  /* method Synchronize */
  function IShellNameSpace/Synchronize () => (), name: "Synchronize", 
        disp-id: 5;

  /* method Import */
  function IShellNameSpace/Import () => (), name: "Import", disp-id: 6;

  /* method Export */
  function IShellNameSpace/Export () => (), name: "Export", disp-id: 7;

  /* method InvokeContextMenuCommand */
  function IShellNameSpace/InvokeContextMenuCommand (arg-strCommand :: 
        <string>) => (), name: "InvokeContextMenuCommand", disp-id: 8;

  /* method MoveSelectionTo */
  function IShellNameSpace/MoveSelectionTo () => (), name: 
        "MoveSelectionTo", disp-id: 9;

  /* Query to see if subscriptions are enabled */
  constant property IShellNameSpace/SubscriptionsEnabled :: <boolean>, 
        name: "SubscriptionsEnabled", disp-id: 10;

  /* method CreateSubscriptionForSelection */
  function IShellNameSpace/CreateSubscriptionForSelection () => (arg-result 
        :: <boolean>), name: "CreateSubscriptionForSelection", disp-id: 11;

  /* method DeleteSubscriptionForSelection */
  function IShellNameSpace/DeleteSubscriptionForSelection () => (arg-result 
        :: <boolean>), name: "DeleteSubscriptionForSelection", disp-id: 12;

  /* old, use put_Root() instead */
  function IShellNameSpace/SetRoot (arg-bstrFullPath :: <string>) => (), 
        name: "SetRoot", disp-id: 13;

  /* options  */
  property IShellNameSpace/EnumOptions :: type-union(<integer>, 
        <machine-word>), name: "EnumOptions", disp-id: 14;

  /* get the selected item */
  property IShellNameSpace/SelectedItem :: <LPDISPATCH>, name: 
        "SelectedItem", disp-id: 15;

  /* get the root item */
  property IShellNameSpace/Root :: <object>, name: "Root", disp-id: 16;

  property IShellNameSpace/Depth :: type-union(<integer>, <machine-word>), 
        name: "Depth", disp-id: 17;

  property IShellNameSpace/Mode :: type-union(<integer>, <machine-word>), 
        name: "Mode", disp-id: 18;

  property IShellNameSpace/Flags :: type-union(<integer>, <machine-word>), 
        name: "Flags", disp-id: 19;

  property IShellNameSpace/TVFlags :: type-union(<integer>, 
        <machine-word>), name: "TVFlags", disp-id: 20;

  property IShellNameSpace/Columns :: <string>, name: "Columns", disp-id: 
        21;

  /* number of view types */
  constant property IShellNameSpace/CountViewTypes :: type-union(<integer>, 
        <machine-word>), name: "CountViewTypes", disp-id: 22;

  /* set view type */
  function IShellNameSpace/SetViewType (arg-iType :: type-union(<integer>, 
        <machine-word>)) => (), name: "SetViewType", disp-id: 23;

  /* collection of selected items */
  function IShellNameSpace/SelectedItems () => (arg-result :: 
        <LPDISPATCH>), name: "SelectedItems", disp-id: 24;

  /* expands item specified depth */
  function IShellNameSpace/Expand (arg-var :: <object>, arg-iDepth :: 
        type-union(<integer>, <machine-word>)) => (), name: "Expand", 
        disp-id: 25;

  /* unselects all items */
  function IShellNameSpace/UnselectAll () => (), name: "UnselectAll", 
        disp-id: 26;
end dispatch-client <IShellNameSpace>;


/* COM class: ShellNameSpace version 0.0
 * GUID: {55136805-B2DE-11D1-B9F2-00A0C98BC547}
 */
define constant $ShellNameSpace-class-id = as(<REFCLSID>, 
        "{55136805-B2DE-11D1-B9F2-00A0C98BC547}");

define function make-ShellNameSpace () => (default-interface :: 
        <IShellNameSpace>)
  /* Translation error: source interface ShellNameSpace not supported. */
  let default-interface = make(<IShellNameSpace>, class-id: 
        $ShellNameSpace-class-id);
  values(default-interface)
end function make-ShellNameSpace;


/* hidden? Dispatch interface: IScriptErrorList version 0.0
 * GUID: {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
 * Description: Script Error List Interface
 */
define dispatch-client <IScriptErrorList>
  uuid "{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}";

  function IScriptErrorList/advanceError () => (), name: "advanceError", 
        disp-id: 10;

  function IScriptErrorList/retreatError () => (), name: "retreatError", 
        disp-id: 11;

  function IScriptErrorList/canAdvanceError () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "canAdvanceError", 
        disp-id: 12;

  function IScriptErrorList/canRetreatError () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "canRetreatError", 
        disp-id: 13;

  function IScriptErrorList/getErrorLine () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getErrorLine", 
        disp-id: 14;

  function IScriptErrorList/getErrorChar () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getErrorChar", 
        disp-id: 15;

  function IScriptErrorList/getErrorCode () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getErrorCode", 
        disp-id: 16;

  function IScriptErrorList/getErrorMsg () => (arg-result :: <string>), 
        name: "getErrorMsg", disp-id: 17;

  function IScriptErrorList/getErrorUrl () => (arg-result :: <string>), 
        name: "getErrorUrl", disp-id: 18;

  function IScriptErrorList/getAlwaysShowLockState () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: 
        "getAlwaysShowLockState", disp-id: 23;

  function IScriptErrorList/getDetailsPaneOpen () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getDetailsPaneOpen", 
        disp-id: 19;

  function IScriptErrorList/setDetailsPaneOpen (arg-fDetailsPaneOpen :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "setDetailsPaneOpen", disp-id: 20;

  function IScriptErrorList/getPerErrorDisplay () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getPerErrorDisplay", 
        disp-id: 21;

  function IScriptErrorList/setPerErrorDisplay (arg-fPerErrorDisplay :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "setPerErrorDisplay", disp-id: 22;
end dispatch-client <IScriptErrorList>;


/* hidden? COM class: CScriptErrorList version 0.0
 * GUID: {EFD01300-160F-11D2-BB2E-00805FF7EFCA}
 */
define constant $CScriptErrorList-class-id = as(<REFCLSID>, 
        "{EFD01300-160F-11D2-BB2E-00805FF7EFCA}");

define function make-CScriptErrorList () => (default-interface :: 
        <IScriptErrorList>)
  let default-interface = make(<IScriptErrorList>, class-id: 
        $CScriptErrorList-class-id);
  values(default-interface)
end function make-CScriptErrorList;


/* hidden? Dispatch interface: ISearch version 0.0
 * GUID: {BA9239A4-3DD5-11D2-BF8B-00C04FB93661}
 * Description: Enumerated Search
 */
define dispatch-client <ISearch>
  uuid "{BA9239A4-3DD5-11D2-BF8B-00C04FB93661}";

  /* Get search title */
  constant property ISearch/Title :: <string>, name: "Title", disp-id: 
        as(<machine-word>, #x60020000);

  /* Get search guid */
  constant property ISearch/Id :: <string>, name: "Id", disp-id: 
        as(<machine-word>, #x60020001);

  /* Get search url */
  constant property ISearch/URL :: <string>, name: "URL", disp-id: 
        as(<machine-word>, #x60020002);
end dispatch-client <ISearch>;


/* hidden? Dispatch interface: ISearches version 0.0
 * GUID: {47C922A2-3DD5-11D2-BF8B-00C04FB93661}
 * Description: Searches Enum
 */
define dispatch-client <ISearches>
  uuid "{47C922A2-3DD5-11D2-BF8B-00C04FB93661}";

  /* Get the count of searches */
  size constant property ISearches/Count :: type-union(<integer>, 
        <machine-word>), name: "Count", disp-id: as(<machine-word>, 
        #x60020000);

  /* Get the default search name */
  constant property ISearches/Default :: <string>, name: "Default", 
        disp-id: as(<machine-word>, #x60020001);

  /* Return the specified search */
  function ISearches/Item (/*optional*/ arg-index :: <object>) => 
        (arg-result :: <ISearch>), name: "Item", disp-id: 
        as(<machine-word>, #x60020002);

  /* Enumerates the searches */
  function ISearches/_NewEnum () => (arg-result :: <LPUNKNOWN>), name: 
        "_NewEnum", disp-id: -4;
end dispatch-client <ISearches>;


/* hidden? Dispatch interface: ISearchAssistantOC version 0.0
 * GUID: {72423E8F-8011-11D2-BE79-00A0C9A83DA1}
 * Description: ISearchAssistantOC Interface
 */
define dispatch-client <ISearchAssistantOC>
  uuid "{72423E8F-8011-11D2-BE79-00A0C9A83DA1}";

  function ISearchAssistantOC/AddNextMenuItem (arg-bstrText :: <string>, 
        arg-idItem :: type-union(<integer>, <machine-word>)) => (), name: 
        "AddNextMenuItem", disp-id: 1;

  function ISearchAssistantOC/SetDefaultSearchUrl (arg-bstrUrl :: <string>) 
        => (), name: "SetDefaultSearchUrl", disp-id: 2;

  function ISearchAssistantOC/NavigateToDefaultSearch () => (), name: 
        "NavigateToDefaultSearch", disp-id: 3;

  function ISearchAssistantOC/IsRestricted (arg-bstrGuid :: <string>) => 
        (arg-result :: <boolean>), name: "IsRestricted", disp-id: 4;

  /* property ShellFeaturesEnabled */
  constant property ISearchAssistantOC/ShellFeaturesEnabled :: <boolean>, 
        name: "ShellFeaturesEnabled", disp-id: 5;

  /* property SearchAssistantDefault */
  constant property ISearchAssistantOC/SearchAssistantDefault :: <boolean>, 
        name: "SearchAssistantDefault", disp-id: 6;

  /* Get searches */
  constant property ISearchAssistantOC/Searches :: <ISearches>, name: 
        "Searches", disp-id: 7;

  /* Returns true if the current folder is web folder */
  constant property ISearchAssistantOC/InWebFolder :: <boolean>, name: 
        "InWebFolder", disp-id: 8;

  function ISearchAssistantOC/PutProperty (arg-bPerLocale :: <boolean>, 
        arg-bstrName :: <string>, arg-bstrValue :: <string>) => (), name: 
        "PutProperty", disp-id: 9;

  function ISearchAssistantOC/GetProperty (arg-bPerLocale :: <boolean>, 
        arg-bstrName :: <string>) => (arg-result :: <string>), name: 
        "GetProperty", disp-id: 10;

  write-only property ISearchAssistantOC/EventHandled :: <boolean>, name: 
        "EventHandled", disp-id: 11;

  function ISearchAssistantOC/ResetNextMenu () => (), name: 
        "ResetNextMenu", disp-id: 12;

  function ISearchAssistantOC/FindOnWeb () => (), name: "FindOnWeb", 
        disp-id: 13;

  function ISearchAssistantOC/FindFilesOrFolders () => (), name: 
        "FindFilesOrFolders", disp-id: 14;

  function ISearchAssistantOC/FindComputer () => (), name: "FindComputer", 
        disp-id: 15;

  function ISearchAssistantOC/FindPrinter () => (), name: "FindPrinter", 
        disp-id: 16;

  function ISearchAssistantOC/FindPeople () => (), name: "FindPeople", 
        disp-id: 17;

  function ISearchAssistantOC/GetSearchAssistantURL (arg-bSubstitute :: 
        <boolean>, arg-bCustomize :: <boolean>) => (arg-result :: 
        <string>), name: "GetSearchAssistantURL", disp-id: 18;

  function ISearchAssistantOC/NotifySearchSettingsChanged () => (), name: 
        "NotifySearchSettingsChanged", disp-id: 19;

  property ISearchAssistantOC/ASProvider :: <string>, name: "ASProvider", 
        disp-id: 20;

  property ISearchAssistantOC/ASSetting :: type-union(<integer>, 
        <machine-word>), name: "ASSetting", disp-id: 21;

  function ISearchAssistantOC/NETDetectNextNavigate () => (), name: 
        "NETDetectNextNavigate", disp-id: 22;

  function ISearchAssistantOC/PutFindText (arg-FindText :: <string>) => (), 
        name: "PutFindText", disp-id: 23;

  constant property ISearchAssistantOC/Version :: type-union(<integer>, 
        <machine-word>), name: "Version", disp-id: 24;

  function ISearchAssistantOC/EncodeString (arg-bstrValue :: <string>, 
        arg-bstrCharSet :: <string>, arg-bUseUTF8 :: <boolean>) => 
        (arg-result :: <string>), name: "EncodeString", disp-id: 25;
end dispatch-client <ISearchAssistantOC>;


/* hidden? Dispatch interface: ISearchAssistantOC2 version 0.0
 * GUID: {72423E8F-8011-11D2-BE79-00A0C9A83DA2}
 * Description: ISearchAssistantOC2 Interface
 */
define dispatch-client <ISearchAssistantOC2>
  uuid "{72423E8F-8011-11D2-BE79-00A0C9A83DA2}";

  function ISearchAssistantOC2/AddNextMenuItem (arg-bstrText :: <string>, 
        arg-idItem :: type-union(<integer>, <machine-word>)) => (), name: 
        "AddNextMenuItem", disp-id: 1;

  function ISearchAssistantOC2/SetDefaultSearchUrl (arg-bstrUrl :: 
        <string>) => (), name: "SetDefaultSearchUrl", disp-id: 2;

  function ISearchAssistantOC2/NavigateToDefaultSearch () => (), name: 
        "NavigateToDefaultSearch", disp-id: 3;

  function ISearchAssistantOC2/IsRestricted (arg-bstrGuid :: <string>) => 
        (arg-result :: <boolean>), name: "IsRestricted", disp-id: 4;

  /* property ShellFeaturesEnabled */
  constant property ISearchAssistantOC2/ShellFeaturesEnabled :: <boolean>, 
        name: "ShellFeaturesEnabled", disp-id: 5;

  /* property SearchAssistantDefault */
  constant property ISearchAssistantOC2/SearchAssistantDefault :: 
        <boolean>, name: "SearchAssistantDefault", disp-id: 6;

  /* Get searches */
  constant property ISearchAssistantOC2/Searches :: <ISearches>, name: 
        "Searches", disp-id: 7;

  /* Returns true if the current folder is web folder */
  constant property ISearchAssistantOC2/InWebFolder :: <boolean>, name: 
        "InWebFolder", disp-id: 8;

  function ISearchAssistantOC2/PutProperty (arg-bPerLocale :: <boolean>, 
        arg-bstrName :: <string>, arg-bstrValue :: <string>) => (), name: 
        "PutProperty", disp-id: 9;

  function ISearchAssistantOC2/GetProperty (arg-bPerLocale :: <boolean>, 
        arg-bstrName :: <string>) => (arg-result :: <string>), name: 
        "GetProperty", disp-id: 10;

  write-only property ISearchAssistantOC2/EventHandled :: <boolean>, name: 
        "EventHandled", disp-id: 11;

  function ISearchAssistantOC2/ResetNextMenu () => (), name: 
        "ResetNextMenu", disp-id: 12;

  function ISearchAssistantOC2/FindOnWeb () => (), name: "FindOnWeb", 
        disp-id: 13;

  function ISearchAssistantOC2/FindFilesOrFolders () => (), name: 
        "FindFilesOrFolders", disp-id: 14;

  function ISearchAssistantOC2/FindComputer () => (), name: "FindComputer", 
        disp-id: 15;

  function ISearchAssistantOC2/FindPrinter () => (), name: "FindPrinter", 
        disp-id: 16;

  function ISearchAssistantOC2/FindPeople () => (), name: "FindPeople", 
        disp-id: 17;

  function ISearchAssistantOC2/GetSearchAssistantURL (arg-bSubstitute :: 
        <boolean>, arg-bCustomize :: <boolean>) => (arg-result :: 
        <string>), name: "GetSearchAssistantURL", disp-id: 18;

  function ISearchAssistantOC2/NotifySearchSettingsChanged () => (), name: 
        "NotifySearchSettingsChanged", disp-id: 19;

  property ISearchAssistantOC2/ASProvider :: <string>, name: "ASProvider", 
        disp-id: 20;

  property ISearchAssistantOC2/ASSetting :: type-union(<integer>, 
        <machine-word>), name: "ASSetting", disp-id: 21;

  function ISearchAssistantOC2/NETDetectNextNavigate () => (), name: 
        "NETDetectNextNavigate", disp-id: 22;

  function ISearchAssistantOC2/PutFindText (arg-FindText :: <string>) => 
        (), name: "PutFindText", disp-id: 23;

  constant property ISearchAssistantOC2/Version :: type-union(<integer>, 
        <machine-word>), name: "Version", disp-id: 24;

  function ISearchAssistantOC2/EncodeString (arg-bstrValue :: <string>, 
        arg-bstrCharSet :: <string>, arg-bUseUTF8 :: <boolean>) => 
        (arg-result :: <string>), name: "EncodeString", disp-id: 25;

  constant property ISearchAssistantOC2/ShowFindPrinter :: <boolean>, name: 
        "ShowFindPrinter", disp-id: 26;
end dispatch-client <ISearchAssistantOC2>;


/* hidden? Dispatch interface: _SearchAssistantEvents version 0.0
 * GUID: {1611FDDA-445B-11D2-85DE-00C04FA35C89}
 */
define dispatch-client <_SearchAssistantEvents>
  uuid "{1611FDDA-445B-11D2-85DE-00C04FA35C89}";

  function _SearchAssistantEvents/OnNextMenuSelect (arg-idItem :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "OnNextMenuSelect", disp-id: 1;

  function _SearchAssistantEvents/OnNewSearch () => (), name: 
        "OnNewSearch", disp-id: 2;
end dispatch-client <_SearchAssistantEvents>;


/* hidden? COM class: SearchAssistantOC version 0.0
 * GUID: {B45FF030-4447-11D2-85DE-00C04FA35C89}
 * Description: SearchAssistantOC Class
 */
define constant $SearchAssistantOC-class-id = as(<REFCLSID>, 
        "{B45FF030-4447-11D2-85DE-00C04FA35C89}");

define function make-SearchAssistantOC () => (default-interface :: 
        <ISearchAssistantOC2>)
  /* Translation error: source interface SearchAssistantOC not supported. 
        */
  let default-interface = make(<ISearchAssistantOC2>, class-id: 
        $SearchAssistantOC-class-id);
  values(default-interface)
end function make-SearchAssistantOC;


/* Pointer definitions: */
define C-pointer-type <VARIANT*> => <VARIANT>; ignorable(<VARIANT*>);
