Module: type-library-module

/* Custom interface: IInternalRef version 0.0
 * GUID: {EE6CCD79-194D-11D3-9024-00C04F78ACF9}
 * Description: IInternalRef Interface
 */
define open vtable-interface <IInternalRef> (<IUnknown>)
  client-class <IInternalRef-client>;
  uuid "{EE6CCD79-194D-11D3-9024-00C04F78ACF9}";

  /* method getInternalRef */
  function IInternalRef/getInternalRef () => (arg-ppInternalRef :: 
        <C-signed-long>), name: "getInternalRef";

  /* method setInternalRef */
  function IInternalRef/setInternalRef (arg-pInternalRef :: 
        <C-signed-long>) => (), name: "setInternalRef";
end vtable-interface <IInternalRef>;

/* Custom interface: IContainer version 0.0
 * GUID: {EE6CCD7A-194D-11D3-9024-00C04F78ACF9}
 * Description: IContainer Interface
 */
define open vtable-interface <IContainer> (<IUnknown>)
  client-class <IContainer-client>;
  uuid "{EE6CCD7A-194D-11D3-9024-00C04F78ACF9}";

  /* method getContainedComponent */
  function IContainer/getContainedComponent () => (arg-ppComponent :: 
        <LPUNKNOWN>), name: "getContainedComponent";
end vtable-interface <IContainer>;

/* Custom interface: IAnimationTiming version 0.0
 * GUID: {7E957F8F-C623-11D4-9E59-0050BAA8920E}
 * Description: IAnimationTiming Interface
 */
define open vtable-interface <IAnimationTiming> (<IUnknown>)
  client-class <IAnimationTiming-client>;
  uuid "{7E957F8F-C623-11D4-9E59-0050BAA8920E}";

  /* method setAnimationClockSlaveMode */
  function IAnimationTiming/setAnimationClockSlaveMode (arg-bClockSlaveMode 
        :: <C-signed-long>) => (), name: "setAnimationClockSlaveMode";

  /* method setHost */
  function IAnimationTiming/setHost (arg-animationCallback :: <VARIANT>) => 
        (), name: "setHost";

  /* method getAnimationDuration */
  function IAnimationTiming/getAnimationDuration () => (arg-pResult :: 
        <C-signed-long>), name: "getAnimationDuration";

  /* method getAnimationClockTime */
  function IAnimationTiming/getAnimationClockTime () => (arg-pResult :: 
        <C-signed-long>), name: "getAnimationClockTime";

  /* method getAnimationFrameRenderTime */
  function IAnimationTiming/getAnimationFrameRenderTime () => (arg-pResult 
        :: <C-signed-long>), name: "getAnimationFrameRenderTime";

  /* method pauseAnimationClock */
  function IAnimationTiming/pauseAnimationClock (arg-bPause :: 
        <C-signed-long>) => (), name: "pauseAnimationClock";

  /* method setAnimationClockTime */
  function IAnimationTiming/setAnimationClockTime (arg-nAbsoluteTimeMSecs 
        :: <C-signed-long>, arg-nMaximumAllowableErrorMSecs :: 
        <C-signed-long>, arg-nMaxRestoreRate :: <C-signed-long>, 
        arg-bPermitRewind :: <C-signed-long>) => (), name: 
        "setAnimationClockTime";
end vtable-interface <IAnimationTiming>;



