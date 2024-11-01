{: This help file explain all the interaction task classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Tasks unit file
   that you must include in the <B=uses> clause in all of your units

   that access the types mentioned here.

   See also <See Class=TCADPrg><BR>

   The task classes defined here can be used in your program by
   adding a <See Class=TCADPrg> component of the desired type
   (2D or 3D) and using the following code to start a task:

   <CODE=
    ACADPrg.StartOperation(<<A task class>>, <<The required task parameter>>);
   >

   If you want to stop a task use the following code:

   <CODE=
    ACADPrg.SendUserEvent(CADPRG_CANCEL);
   >

   and to finish the current task use the code:

   <CODE=
    ACADPrg.SendUserEvent(CADPRG_ACCEPT);
   >

   You may also start another task by suspending the current
   one (if it can be suspended) with the code:

   <CODE=
    ACADPrg.SuspendOperation(<<A task class>>, <<The required task parameter>>);
   >

   <B=Note>: All the 3D tasks work on the active
   <See=working plane@WORKPLANE> of the <See Class=TCADPrg3D>.
}
unit CS4Tasks;

Interface

uses SysUtils, Classes, Graphics, Dialogs, Types, LCLType,
     CADSys4, CS4BaseTypes, CS4Shapes,
     clipper,
     CADSys4ClipperInterface,
     fSetKerftype;


//added
procedure CopyToCADClipboard(AIterator: TGraphicObjIterator);
procedure Inverse(APrimitive2D: TPrimitive2D);
procedure InverseEllipticalArc2D(AArcD: TEllipticalArc2D);
procedure Reverse(APrimitive2D: TPrimitive2D);
procedure ReverseEllipticalArc2D(var AArcD: TEllipticalArc2D);
procedure ReverseLine2D(ALine2D: TLine2D);
procedure ReverseOutLine2D(AOutLine2D: TOutLine2D);

type

TCAD2DDeleteObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DScaleObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DMirrorXObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DMirrorYObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DExplodeObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DReverse = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DInverse = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DCreateSourceBlock = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const AStateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DBringToFront = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DSendToBack = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DBringForward = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DSendBackwards = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DSwapObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DAlignLeft = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DAlignTop = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DAlignRight = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DAlignBottom = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DCopyObjectsToCADClipboard = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DCutObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DPasteObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DMakeContainer = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DOffsetObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

//Clipper
TCAD2DClipperUnionObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DClipperIntersectionObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DClipperXOrObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DClipperDifferenceObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

TCAD2DClipperOffsetObjects = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;
//End-Clipper

//CAM
TCAD2DSetKerftypes = class(TCADState)
public
  constructor Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
end;

//end-added

type
  { ******************* Zooming states *********************** }

  {: This class rapresents the parameter for zooming tasks.

     All the zooming tasks may use an optional parameter that
     is useful only if you want to start an interaction task
     after the zooming task.
  }
  TCADPrgZoomParam = class(TCADPrgParam);

  {: This is the base class for all zooming and panning
     operations.

     Because it is an abstract class it cannot be used as an
     operation. It is only defined to give a
     common interface for zooming tasks.

     See also <See Class=TCADPrg>.
  }
  TCADPrgZoomState = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class can be used to perform a windowed zoom.

     The operation waits two points from the user that are
     the two window's corner of the area to be zoommed.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button
     is not used.

     To stop the operation you can use either the
     <See Method=TCADPrg@StopOperation> method or send the
     <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgZoomArea = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class can be used to perform a <I=continuos> zoom in/zoom out.

     The operation waits for the zooming center point. If the
     shift key is hold while pressing the mouse left button, a
     zoom out will be performed, otherwise a zoom in will be done.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button is
     not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot
     be suspended.
  }
  TCADPrgZoomInOut = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  {: This class can be used to perform a single pan operation.

     The operation waits two points from the user that are the
     start and ending point of the panning. After the selection
     of the two points, the current <I=visual rect> will be
     translated from the start point to the end point.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button
     is not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgPan = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class can be used to perform a dynamic pan operation.

     The operation waits for the mouse to be pressed. Then,
     while holding the mouse left button, the current
     <I=visual rect> will follow the mouse position. If the
     draws is complex and the painting thread is not enabled the
     panning can be delayed with respect to the mouse movements.

     The right mouse button is not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message. The operation doesn't end
     by itself but you have to send a <I=CADPRG_ACCEPT> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgRealTimePan = class(TCADPrgZoomState)
  private
    fInPanning: Boolean;
    fLastPoint: TPoint2D;
    fOriginalRect: TRect2D;
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  { ******************* Useful states *********************** }

  {: This is the base class for the selection operations.

     Because it is an abstract class it cannot be used as an
     operation.

     See also <See Class=TCADPrg>.
  }
  TCADPrgSelectionState = class(TCADState);

  {: This class defines the parameter needed by the
     <See Class=TCADPrgSelectArea> task.

     The parameter is used to store the area selected by the
     user.
  }
  TCADPrgSelectAreaParam = class(TCADPrgParam)
  private
    fFrame: TFrame2D;
    fCallerParam: TCADPrgParam;
    function GetArea: TRect2D;
  public
    {: This constructor creates the instance of the parameter
       to be passed to <See Method=TCADPrg@StartOperation> or
       <See Method=TCADPrg@SuspendOperation>.

       <I=AfterS> contains the starting state of the operation
       that can be started at the end of the selection. If it
       is <B=nil>, the CADPrg will returns into the default
       state.

       <I=CallerParam> may contains an optional parameter that
       can be used by the operation that will receive the selection.
       This parameter will be freed when the parameter will be
       deleted. If you need it after the deletion of the
       parameter set it to <B=nil> after you have retrieved it.
    }
    constructor Create(AfterS: TCADStateClass; CallerParam: TCADPrgParam);
    destructor Destroy; override;
    {: This property contains an optional parameter that can
       be used by the operation that will recive the selection made.

       This parameter will be freed when the parameter will be
       deleted. If you need it after the deletion of the
       parameter set it to <B=nil> after you have retrieved it.
    }
    property CallerParam: TCADPrgParam read fCallerParam write fCallerParam;
    {: It will contains the area selected by the user.
    }
    property Area: TRect2D read GetArea;
  end;

  {: This task class allows the user to select a 2D area defined
     in viewport coordinates.

     It can be used to obtain a rectangular parameter usable in
     selections or other operations.

     The operation waits for two points from the user that are
     the two area corner.
     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button is
     not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation requires a <See Class=TCADPrgSelectAreaParam>
     parameter.

     See also <See Class=TCADPrg>.
  }
  TCADPrgSelectArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD2DCommonParam = class(TCADPrgParam);

  {: This class defines the parameter used by the
     <See Class=TCAD2DPositionObject> task.

     The class is used to store the 2D object that must be
     positioned on the CAD. If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DPositionObjectParam = class(TCAD2DCommonParam)
  private
    fObject: TObject2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=O> contains the object to be positioned with the
        <See Class=TCAD2DPositionObject> task.>

        <B=Note>: in the case <I=AfterS> is not <B=nil>, the
         object will not be added to the CAD.
    }
    constructor Create(AfterS: TCADStateClass; O: TObject2D);
    {: This property contains the object to be positioned
       (or already positioned when the task is finished) with the
       <See Class=TCAD2DPositionObject> task.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TCADCmp>,
       otherwise it will be added to it.
    }
    property Obj: TObject2D read fObject;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DDrawUnSizedPrimitive> to construct a 2D
     primitive with a variable number of <I=control points>,
     like a polyline.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DDrawUnSizedPrimitiveParam = class(TCAD2DCommonParam)
  private
    fPrimObject: TPrimitive2D;
    fCurrPoint: Word;
    fOrtoIsUsable: Boolean;
  protected
    {: This method updates the on-screen informations during
       the task.

       The method draws the current primitive with the rubber
       band (xor pen mode) mode. You will see your primitive
       growing as new points are added to it.
    }
    procedure DrawOSD(Viewport: TCADViewport2D);
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 2D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal to
        3, when the user click on the viewport, the fourth control
        point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any means with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
    {: This property contains the primitive being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TCADCmp>,
       otherwise it will be added to it.
    }
    property Primitive: TPrimitive2D read fPrimObject;
    {: This property indicates if the ortogonal constraint has
       any means with the primitive being defined.

       If it is <B=True>, the orto constraint will be used,
       otherwise it will not used.
    }
    property OrtoIsUsable: Boolean read fOrtoIsUsable write fOrtoIsUsable;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DDrawSizedPrimitive> to construct a 2D
     primitive with a fixed number of points, like an ellipse.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DDrawSizedPrimitiveParam = class(TCAD2DDrawUnSizedPrimitiveParam)
  private
    fnPoints: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 2D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal
        to 3 when the user click on the viewport, the fourth
        control point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any meanse with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
  end;

  {: This class implements the <I=object positioning task>.

     This task may be used to add an object to the linked CAD
     by firstly positioning it in the world.

     The operation wait for the user to do the following
     (in the order given here):

     <LI=move the object to the desired position using the
      mouse. You will see the object moving on the screen.>
     <LI=press the left mouse button on that point to lay down
      the object.>

     The object will be moved using its bottom-left bounding box
     corner as the base point. At the end of the task the object
     is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DPositionObjectParam> class. The task may
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DPositionObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a fixed number of control points.

     This task may be used to add an object to the linked CAD by
     firstly defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the
      parameter is greater than zero the control point to be
      positioned is not the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second step until no
      control points are left.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
     added to the CAD.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a variable number of control points.

     This task may be used to add an object to the linked CAD
     by firstly defining its control points.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the parameter
      is greater than zero the control point to be positioned is not
      the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.
     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD. Note that this is the only way to end
      the task.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawUnSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawUnSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <See Class=TCAD2DDrawArcPrimitive> task to construct a
     2D arc segment.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DDrawArcPrimitiveParam = class(TCAD2DCommonParam)
  private
    fArcObject: TEllipticalArc2D;
    fCurrPoint: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state. Note that in case
        <I=AfterS> is not <B=nil>, the object will not be added to
        the CAD.>
       <LI=<I=Arc> contains the object to be constructed with
        the <See Class=TCAD2DDrawArcPrimitive> task.>
    }
    constructor Create(AfterS: TCADStateClass; Arc: TEllipticalArc2D);
    {: This property contains the 2D arc that is being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.
    }
    property Arc: TEllipticalArc2D read fArcObject;
  end;

  {: This class implements the <I=arc constructing task>.

     This task may be used to add an arc to the linked CAD by
     defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the desired position for the first
      control point of the arc.>
     <LI=press the left mouse button to set the first control
      point on that point.>
     <LI=move the mouse on the desired position for the second
      control point. You will see an ellipse drawed on the
      viewport.>
     <LI=press the left mouse button to set the second control
      point.>
     <LI=move the mouse on the desired position for the third
      control point. You will modify the starting angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the third control
      point.>
     <LI=move the mouse on the desired position for the fourth
      control point. You will modify the ending angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the fourth control
      point.>

     At the end of the task the arc is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD.>
     <LI=CADPRG_CANCEL>. The task is aborted and the object
     destroyed. The CADPrgreturns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawArcPrimitiveParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawArcPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD2DSelectObjectsParam = class;

  {: This type defines the prototype for an event handler used
     to inform the application that an object was picked.

     See <See Class=TCAD2DSelectObject>,
     <See Class=TCAD2DSelectObjects> and
     <See Class=TCAD2DSelectObjectsInArea> for details.

     Parameters:

     <LI=<I=Sender> contains the instance of
      <See Class=TCAD2DSelectObjectsParam> (or
      <See Class=TCAD2DSelectObjectsInAreaParam>) of the task
      that had called the handler.>
     <LI=<I=Obj> contains the picked object, or <B=nil> if the
      selection task is <See Class=TCAD2DSelectObjectsInArea>.>
     <LI=<I=CtrlPt> contains the control point on which the
      mouse was picked. This is the same result of the
      <See Method=TCADViewport2D@PickObject> method. This
      parameter will be <I=PICK_NOOBJECT> in case the selection
      task is <See Class=TCAD2DSelectObjectsInArea>.>
     <LI=<I=Added> is <B=True> if Obj is added to the selected
      object list, or <B=False> if it is removed.>

     If a repaint event is raised, the handler is called for all
     the picked objects. In this case the <I=CtrlPt> is
     <I=PICK_NOOBJECT> and <I=Added> is <B=True> the selection
     task that fired the event is
     <See Class=TCAD2DSelectObjectsInArea>.
  }
  TSelection2DEvent = procedure(Sender: TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer; Added: Boolean) of object;

  {: This class defines the parameter used by
     <See Class=TCAD2DSelectObject> and
     <See Class=TCAD2DSelectObjects> to pick objects
     interactively on the screen.
  }
  TCAD2DSelectObjectsParam = class(TCAD2DCommonParam)
  private
    fApertureSize: Word;
    fEndIfNoObject, fEndWithMouseDown: Boolean;
    fLastSelectedCtrlPoint: Integer;
    fLastPt: TPoint2D;
    fSelectedObjs: TGraphicObjList;
    fOnSelected: TSelection2DEvent;
    fSelectionFilter: TObject2DClass;
  protected
    {: This method draws the picking frame used to show the
       aperture of the picking.

       The frame is drawed in xor pen mode and centered at <I=Pt>.

       Parameters:

       <LI=<I=Viewport> is the viewport on which draw the frame.>
       <LI=<I=Pt> is the point at which draw the frame.>
    }
    procedure DrawOSD(Viewport: TCADViewport2D; const Pt: TPoint2D); virtual;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=ApertureSize> is the aperture used for the picking
        in pixels.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.
    }
    constructor Create(ApertureSize: Word; const AfterS: TCADStateClass);
    destructor Destroy; override;
    {: This property is the list that contains the picked
       objects.

       If you want to traverse the list ask for an iterator and
       remember to free it before to returns to the selection
       task.

       See also <See Class=TGraphicObjList>.
    }
    property SelectedObjects: TGraphicObjList read fSelectedObjs;
    {: This property contains the control point selected of the
       last picked object.
    }
    property LastSelectedCtrlPoint: Integer read fLastSelectedCtrlPoint;
    {: This property may contain a class reference type (deriving
       from <See Class=TObject2D>) used to filter the selection.

       If the picked object doesn't derive from that class, the
       object is ignored.

       By default it is <I=TObject2D>.
    }
    property SelectionFilter: TObject2DClass read fSelectionFilter write fSelectionFilter;
    {: This property specifies if the selection task must be ended
       when the <I=mouse down> event is received.

       If it is <I=True> the task is finished as soon as the
       user press the mouse. Otherwise the task will finish when
       the user release the mouse button.

       By default it is <B=False>.
    }
    property EndSelectionWithMouseDown: Boolean read fEndWithMouseDown write fEndWithMouseDown;
    {: This property specifies if the selection task must be
       ended when the user has pressed the mouse button but
       not object is under the mouse.

       By default it is <B=False>.

       <B=Note>: This property is meaningfull only with multiple
        selection.
    }
    property EndSelectionIfNoObject: Boolean read fEndIfNoObject write fEndIfNoObject;
    {: EVENTS}
    {: This property may contains an event handler that will be
      called when an object is picked (after it was added to the
      list).

      See Also <See Type=TSelection2DEvent>.
    }
    property OnObjectSelected: TSelection2DEvent read fOnSelected write fOnSelected;
  end;

  {: This class implements the <I=single object selection task>.

     This task may be used to select an object of the linked CAD.
     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>

     The object will be added to the
     <See Property=TCAD2DSelectObjectsParam@SelectedObjects> list
     of the task parameter. Normally you set <I=AfterS> of the
     task parameter to a state that will process the selected
     object by using the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=multiple object selection task>.

     This task may be used to select a set of object of the
     linked CAD.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object. If the object was already picked, it is removed
      from <See Property=TCAD2DSelectObjectsParam@SelectedObjects>
      list of the task parameter, otherwise it will be added.>
     <LI=continue with the first step.>

     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     Note that no visual feedback is given to the user. If you
     want to show the selected objects, you can use the
     <See Property=TCAD2DSelectObjectsParam@OnObjectSelected>
     handler of the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>
     <LI=<I=CADPRG_ACCEPT>. The task is ended. The CADPrg
     returns in the default state or in the state specified by
     <I=AfterS>. Note that this is the only way to finish the
     task.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines a special kind of selection task that
     is the combination of the
     <See Class=TCAD2DSelectObject> and
     <See Class=TCAD2DSelectObjects> tasks.

     If the user holds down the Shift key, the task behaves
     like the <I=TCAD2DSelectObjects> task, otherwise it
     behaves like the <I=TCAD2DSelectObject> task.

     See also <See Class=TCADPrg>.
  }
  TCAD2DExtendedSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DSelectObjectsInArea> task to select the
     objects contained in the specified window area.
  }
  TCAD2DSelectObjectsInAreaParam = class(TCAD2DSelectObjectsParam)
  private
    fAreaMode: TGroupMode;
    fArea: TRect2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AreaMode> specify the type of selection. If it is
        <I=gmAllInside> only the objects fully contained in the
        area are selected; if it is <I=gmCrossFrame> all the
        objects that are contained or cross the area are selected.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is nil, the
        CADPrg will return to the default state.>
    }
    constructor Create(AreaMode: TGroupMode; const AfterS: TCADStateClass);
  end;

  {: This class implements <I=the 'area selection task>.

     This task may be used to select a set of object of
     the linked CAD by specify a rectangle frame.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the first corner of the area.>
     <LI=press the left mouse button to accept the point.>
     <LI=move the mouse on the second corner of the area. You
      will see the area being defined on the screen.>
     <LI=press the left mouse button to accept the point.>

     All the objects in the area are selected and stored in the
     <See Property=TCAD2DSelectObjectsParam@SelectedObjects>
     list of the task parameter.
     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsInAreaParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObjectsInArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <I=object transformation task>.

     All the transformations of objects that may be described
     by a matrix transform that accept as parametesr a base
     point and a moving point can be modelled by this task.
     You only need to derive from this parameter and
     redefine the
     <See Method=TCAD2DTransformObjectsParam@GetTransform> method.
     Then you can pass the new parameter to
     <See Class=TCAD2DTransformObjects> task.
  }
  TCAD2DTransformObjectsParam = class(TCAD2DCommonParam)
  private
    fBasePt: TPoint2D;
    fNPoint: Integer;
    fBox: TRect2D;
    fObjs: TGraphicObjList;
    fUseFrame: Boolean;
    fCurrTransf: TTransf2D;

    procedure TransformObjs(CurrPt: TPoint2D; UseOrto: Boolean);
    procedure ConfirmTransform;
    procedure CancelTransform;
  protected
    {: This method draws the bounding box of the set of objects
       to be transformed.

       It is used by the <See Method=TCAD2DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithFrame(Viewport: TCADViewport2D); dynamic;
    {: This method draws the objects to be transformed in
       rubber band mode (xor pen mode).

       It is used by the <See Method=TCAD2DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithoutFrame(Viewport: TCADViewport2D); dynamic;
    {: This is the key method of the class.

       It must return the matrix transform that define the
       transformation of the objects. The returned matrix will
       override the current model transform for the selected
       objects.

       This method must be redefined in the derived classes
       for specific transformations.

       Parameters:

       <LI=<I=BasePt> is the base point for the transformation.
        For example to rotate an object you must give the center
        of rotation; to move an object you must give the first
        point of the translation.>
       <LI=<I=CurrPt> is the current point of the mouse. You
        may use this point to define the current transformation.
        For example to rotate an object you must give a second
        point, so you are able to find the angle of rotation
        with respect to the <I=BasePt>; to move an object this
        point is the second point of the translation.>
    }
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D; virtual; abstract;
    {: This method draws the on screen informations that informs
       the user of the result of the transformation.

       There are two modes of visualization:

       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawOSD(Viewport: TCADViewport2D);
    {: This property contains the base point for the
       transformation.
    }
    property BasePoint: TPoint2D read fBasePt;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=Objs> is a list that contains the objects to be
        transformed. The list must have the
        <See Property=TGraphicObjList@FreeOnClear> property set
        to <B=False>.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.>
    }
    constructor Create(Objs: TGraphicObjList; const AfterS: TCADStateClass);
    destructor Destroy; override;
    {: This property contains the list of the objects to be
       transformed.
    }
    property Objects: TGraphicObjList read fObjs;
    {: This property selects the visualization mode for the on
       screen informations:

       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       When the parameter is constructed, this property is set
       to <B=False> if the passed list of objects has only one
       object, it is set to <B=True> otherwise.>
    }
    property UseFrame: Boolean read fUseFrame write fUseFrame;
  end;

  {: This class defines the method
     <See Method=TCAD2DTransformObjectsParam@GetTransform> for
     the <I=move object task>.
  }
  TCAD2DMoveObjectsParam = class(TCAD2DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D; override;
  end;

  {: This class defines the method
     <See Method=TCAD2DTransformObjectsParam@GetTransform> for
     the <I=rotate object task>.
  }
  TCAD2DRotateObjectsParam = class(TCAD2DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D; override;
  end;

  {: This class implements the <I=transform objects task>.

     This task may be used to apply a transformation to a set
     of objects by specifing the appropriate parameter (see below).

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the transformation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      object transformed on the screen.>
     <LI=press the left mouse button to accept the current
      transformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of a class derived from
     <See Class=TCAD2DTransformObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DTransformObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=move a selection task>.

     This task may be used to select and move a set of objects
     by specifing the start point and end point of the
     translation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the translation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      objects moving on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD2DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DMoveSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate a selection task>.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD2DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DRotateSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  TCAD2DRotateExSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;


  {: This class implements the <I=edit primitive task>.

     This task may be used to move the control points of a
     <See Class=TPrimitive2D> interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the new
      position of the control point.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=CADPRG_CANCEL>. The task is aborted and the parameter
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the of the primitive
     incapsulated into the <See Property=TCADPrgParam@UserObject>
     property of a <See Class=TCADPrgParam> instance.

     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DEditPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=select and edit primitive task>.

     This task may be used to select a primitive and move its
     control points interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>
     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the
      new position of the control point.>
     <LI=continue from the third step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD2DSelectObjectsParam>.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DEditSelectedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;


implementation

uses Math;



type
// -----===== Starting Cs4CADPrgTasks.pas =====-----
  TCADPrgEndZoomArea = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  TCADPrgDragPan = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  TCADPrgDragSelectArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

// -----===== Starting Cs4CADPrgTasks2D.pas =====-----
  TCAD2DEditPrimitiveParam = class(TCAD2DCommonParam)
  private
    fCurrentPrimitive, fOriginalPrimitive: TPrimitive2D;
    fCurrentCtrlPt: Integer;
    fApertureSize: Word;
    fLastPt: TPoint2D;
  public
    constructor Create(Prim: TPrimitive2D; ApertureSize: Word);
    destructor Destroy; override;

    procedure SetCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
    procedure AddCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
    procedure UnSetCtrlPoint;
    procedure AcceptEdited;
    procedure MoveCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
    procedure DrawOSD(Viewport: TCADViewport2D; Pt: TPoint2D; FirstTime: Boolean);
    procedure DrawModifiedPrim(Viewport: TCADViewport2D);

    property CurrentCtrlPt: Integer read fCurrentCtrlPt;
  end;

// -----===== Starting Cs4CADPrgTasks.pas =====-----

{ -------------- TCADPrgSelectAreaParam -------------- }
constructor TCADPrgSelectAreaParam.Create(AfterS: TCADStateClass; CallerParam: TCADPrgParam);
begin
  inherited Create(AfterS);
  fFrame := TFrame2D.Create(0, Point2D(0, 0), Point2D(0, 0));
  fCallerParam := CallerParam;
end;

destructor TCADPrgSelectAreaParam.Destroy;
begin
  fFrame.Free;
  inherited Destroy;
end;

function TCADPrgSelectAreaParam.GetArea: TRect2D;
var
  Pt: TPoint2D;
begin
  Pt := fFrame.Box.FirstEdge;
  Result := fFrame.Box;
end;

{ ******************* Useful states *********************** }

{ ------------------ Select Area --------------------- }

constructor TCADPrgSelectArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the first point of the area.'
end;

{ Need TCADPrgSelectAreaParam. }
function TCADPrgSelectArea.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton;
                                  Shift: TShiftState; Key: Word;
                                  var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with Param as TCADPrgSelectAreaParam, CADPrg do
   case Event of
    ceUserDefined:
     if Key = CADPRG_CANCEL then
      begin
         Param.Free;
         Param := nil;
         NextState := CADPrg.DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     begin
      CurrPoint := CurrentViewportPoint;
      fFrame.Points[0] := CurrPoint;
      fFrame.Points[1] := CurrPoint;
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
      NextState := TCADPrgDragSelectArea;
      Result := True;
     end;
   end;
end;

procedure TCADPrgSelectArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCADPrgDragSelectArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the second point of the area.'
end;

function TCADPrgDragSelectArea.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton;
                                      Shift: TShiftState; Key: Word;
                                      var NextState: TCADStateClass): Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with Param as TCADPrgSelectAreaParam, CADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      if Assigned(AfterState) then
       NextState := AfterState
      else
       NextState := DefaultState;
      Result := True;
    end;
    ceMouseMove: begin
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
      fFrame.Points[1] := CurrentViewportPoint;
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
    end;
    cePaint:
    if Viewport is TCADViewport2D then
     TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
   end;
end;

procedure TCADPrgDragSelectArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ******************* Zooming states *********************** }

constructor TCADPrgZoomState.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  CanBeSuspended := False;
end;

{ ------------------ Zoom Area --------------------- }

{ No parameter. }
constructor TCADPrgZoomArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Param := TCADPrgSelectAreaParam.Create(TCADPrgEndZoomArea, StateParam);
  NextState := TCADPrgSelectArea;
end;

constructor TCADPrgEndZoomArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if Assigned(Param) then
   with CADPrg as TCADPrg, Param as TCADPrgSelectAreaParam do
    begin
      if not IsSamePoint2D(Area.FirstEdge, Area.SecondEdge) then
       Viewport.ZoomWindow(Area);
      if (CallerParam is TCADPrgZoomParam) and
         Assigned(TCADPrgParam(CallerParam).AfterState) then
       NextState := TCADPrgParam(CallerParam).AfterState
      else
       NextState := CADPrg.DefaultState;
      Param.Free;
      Param := nil;
    end;
end;

{ ------------------ ZoomInOut --------------------- }

constructor TCADPrgZoomInOut.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the center of the zoom. (Hold Shift key for zoom out)'
end;

{ No parameter. }
function TCADPrgZoomInOut.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                 var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  with CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      CurrPoint := CurrentViewportPoint;
      with Viewport do
       PanWindow(CurrPoint.X - (VisualRect.Right + VisualRect.Left) / 2.0,
               CurrPoint.Y - (VisualRect.Bottom + VisualRect.Top) / 2.0);
      if ssShift in Shift then
       Viewport.ZoomOut
      else
       Viewport.ZoomIn;
     end;
   end;
end;

{ ------------------ Pan --------------------- }

{ No parameter. }
constructor TCADPrgPan.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Param := TCADPrgParam.Create(StateParam.AfterState);
  Param.UserObject := TLine2D.Create(0, Point2D(0, 0), Point2D(0, 0));
  Description := 'Select the start point of the pan.'
end;

function TCADPrgPan.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                           var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with TCADPrgParam(Param), CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         TLine2D(UserObject).Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     begin
       CurrPoint := CurrentViewportPoint;
       TLine2D(UserObject).Points[0] := CurrPoint;
       TLine2D(UserObject).Points[1] := CurrPoint;
       if Viewport is TCADViewport2D then
        TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
       NextState := TCADPrgDragPan;
       Result := True;
     end;
   end;
end;

procedure TCADPrgPan.OnStop;
begin
  TCADPrgParam(Param).UserObject.Free;
  Param.Free;
  Param := nil;
end;

constructor TCADPrgDragPan.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the end point of the pan.'
end;

function TCADPrgDragPan.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                               var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with TCADPrgParam(Param), CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         TLine2D(UserObject).Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     with TLine2D(UserObject) do
      begin
        IgnoreEvents := True;
        try
         if not IsSamePoint2D(Box.FirstEdge, Box.SecondEdge) then
         Viewport.PanWindow(Points[0].X - Points[1].X, Points[0].Y - Points[1].Y);
        finally
         IgnoreEvents := False;
        end;
        if Assigned(AfterState) then
         NextState := AfterState
        else
         NextState := DefaultState;
        Free;
        Param.Free;
        Param := nil;
        Result := True;
      end;
    ceMouseMove: begin
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
      CurrPoint := CurrentViewportPoint;
      TLine2D(UserObject).Points[1] := CurrPoint;
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
    end;
    cePaint:
    if Viewport is TCADViewport2D then
     TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
   end;
end;

procedure TCADPrgDragPan.OnStop;
begin
  TCADPrgParam(Param).UserObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------------ RealTimePan --------------------- }

{ No parameter. }
constructor TCADPrgRealTimePan.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Old the mouse and move it to pan.';
  fInPanning := False;
  fOriginalRect := CADPrg.Viewport.VisualRect;
end;

function TCADPrgRealTimePan.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  ScrPt, CurrPoint: TPoint2D;
  TmpDist, RefDist: TRealType;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  with CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         NextState := DefaultState;
         Result := True;
         Viewport.ZoomWindow(fOriginalRect);
       end
      else if Key = CADPRG_ACCEPT then
       begin
         if (Param is TCADPrgZoomParam) and Assigned(TCADPrgParam(Param).AfterState) then
          NextState := TCADPrgParam(Param).AfterState
         else
          NextState := DefaultState;
         RepaintAfterOperation;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     begin
       fInPanning := True;
       fLastPoint := CurrentViewportPoint;
       if MouseButton = cmbRight then
        begin
          NextState := DefaultState;
          Result := True;
        end;
     end;
    ceMouseDblClick,
    ceMouseUp: if MouseButton = cmbLeft then
     begin
       if Viewport.UsePaintingThread then
        Viewport.Repaint
       else
        Viewport.Refresh;
      fInPanning := False;
     end;
    ceMouseMove: if fInPanning then
     begin
       Viewport.StopRepaint;
       CurrPoint := CurrentViewportPoint;
       TmpDist := PointDistance2D(CurrPoint, fLastPoint);
       RefDist := PointDistance2D(Viewport.VisualRect.FirstEdge, Viewport.VisualRect.SecondEdge);
       if (TmpDist < RefDist * 0.0001) or (TmpDist > RefDist * 2) then
        begin
          fLastPoint := CurrPoint;
          Exit;
        end;
       ScrPt := Viewport.ViewportToScreen(CurrPoint);
       Viewport.PanWindow(fLastPoint.X - CurrPoint.X, fLastPoint.Y - CurrPoint.Y);
       fLastPoint := Viewport.ScreenToViewport(ScrPt);
     end;
   end;
end;

// -----===== Starting Cs4CADPrgTasks2D.pas =====-----

{ ******************* Drawing tasks *********************** }

constructor TCAD2DPositionObjectParam.Create(AfterS: TCADStateClass; O: TObject2D);
begin
  inherited Create(AfterS);
  fObject := O;
end;

constructor TCAD2DDrawUnSizedPrimitiveParam.Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
begin
  inherited Create(AfterS);
  fPrimObject := Primitive;
  fCurrPoint := StartPointIdx;
  fOrtoIsUsable := OrtoIsU;
end;

procedure TCAD2DDrawUnSizedPrimitiveParam.DrawOSD(Viewport: TCADViewport2D);
begin
  Viewport.DrawObject2DWithRubber(fPrimObject, True);
end;

constructor TCAD2DDrawSizedPrimitiveParam.Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
begin
  inherited Create(AfterS, Primitive, StartPointIdx, OrtoIsU);
  fnPoints := Primitive.Points.Count;
end;

{ ------------- }

constructor TCAD2DPositionObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DPositionObjectParam) then
   Raise ECADSysException.Create('TCAD2DPositionObject: Invalid param');
  Description := 'Press the mouse on the desired insertion point.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DPositionObject.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                      var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DPositionObjectParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         fObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      if Assigned(AfterState) then
       begin
         NextState := AfterState;
         Result := True;
         Exit;
       end;
      IgnoreEvents := True;
      TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
      Viewport2D.CADCmp2D.DrawOnAdd := True;
      try
       Viewport2D.CADCmp2D.AddObject(fObject.ID, fObject);
      finally
        Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
        IgnoreEvents := False;
      end;
      fObject.UpdateExtension(Self);
      Param.Free;
      Param := nil;
      NextState := DefaultState;
      Result := True;
      Exit;
    end;
    ceMouseMove: begin
      CurrPoint2D := CurrentViewportSnappedPoint;
      if not IsSameTransform2D(fObject.ModelTransform, IdentityTransf2D) then
       Viewport2D.DrawObject2DWithRubber(fObject, True);
      fObject.MoveTo(CurrPoint2D, fObject.Box.FirstEdge);
      Viewport2D.DrawObject2DWithRubber(fObject, True);
      //added
      //if (fObject = TBitmap2D) then
        //Viewport2D.Repaint;
    end;
    cePaint: if not IsSameTransform2D(fObject.ModelTransform, IdentityTransf2D) then
     Viewport2D.DrawObject2DWithRubber(fObject, True);
   end;
end;

procedure TCAD2DPositionObject.OnStop;
begin
  TCAD2DPositionObjectParam(Param).fObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawSizedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DDrawSizedPrimitiveParam) then
   Raise ECADSysException.Create('TCAD2DDrawSizedPrimitive: Invalid param');
  Description := 'Press the mouse on the desired points.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DDrawSizedPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  Cont: Integer;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawSizedPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, fPrimObject);
         finally
          Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
          IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fPrimObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D);
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      SnapOriginPoint := CurrentViewportSnappedPoint;
      if fCurrPoint = 0 then
       for Cont := 0 to fnPoints - 1 do
        fPrimObject.Points[Cont] := CurrPoint2D
      else
       begin
         if fOrtoIsUsable and UseOrto then
          MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
         fPrimObject.Points[fCurrPoint] := CurrPoint2D;
       end;
      Inc(fCurrPoint);
      if fCurrPoint = fnPoints then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          // ADDITIONAL CODE
                    //if fprimObject.ClassName = 'TDimension2D' then
                     //Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, (fprimObject as TDimension2D).txt);
          // ADDITIONAL CODE END
          Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
         Exit;
       end
      else
       fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      if fOrtoIsUsable and UseOrto then
       MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
      DrawOSD(Viewport2D);
      fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    cePaint: DrawOSD(Viewport2D);
   end;
end;

procedure TCAD2DDrawSizedPrimitive.OnStop;
begin
  TCAD2DDrawSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawUnSizedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DDrawUnSizedPrimitiveParam) then
   Raise ECADSysException.Create('TCAD2DDrawUnSizedPrimitive: Invalid param');
  TCAD2DDrawUnSizedPrimitiveParam(StateParam).fPrimObject.Points.Clear;
  Description := 'Press the mouse on the desired points.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DDrawUnSizedPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawUnSizedPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fPrimObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D);
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      SnapOriginPoint := CurrentViewportSnappedPoint;
      if fCurrPoint = 0 then
       fPrimObject.Points.Add(CurrPoint2D)
      else
       begin
         if fOrtoIsUsable and UseOrto then
          MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
         fPrimObject.Points[fCurrPoint] := CurrPoint2D;
       end;
      if (fCurrPoint = 0) or not IsSamePoint2D(CurrPoint2D, fPrimObject.Points[fCurrPoint - 1]) then
       Inc(fCurrPoint);
      fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      if fOrtoIsUsable and UseOrto then
       MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
      DrawOSD(Viewport2D);
      fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    cePaint: DrawOSD(Viewport2D);
   end;
end;

procedure TCAD2DDrawUnSizedPrimitive.OnStop;
begin
  TCAD2DDrawUnSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawArcPrimitiveParam.Create(AfterS: TCADStateClass; Arc: TEllipticalArc2D);
begin
  inherited Create(AfterS);
  fArcObject := Arc;
end;

constructor TCAD2DDrawArcPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DDrawArcPrimitiveParam) then
   Raise ECADSysException.Create('TCAD2DDrawArcPrimitive: Invalid param');
  Description := 'Drag the ellipse which contain the arc.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DDrawArcPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawArcPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fArcObject.ID, fArcObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fArcObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
      CurrPoint2D := fArcObject.WorldToObject(CurrentViewportSnappedPoint);
      SnapOriginPoint := CurrentViewportSnappedPoint;
      fArcObject.Points[fCurrPoint] := CurrPoint2D;
      Inc(fCurrPoint);
      if fCurrPoint = 4 then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         IgnoreEvents := True;
         try
          Viewport2D.CADCmp2D.AddObject(fArcObject.ID, fArcObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
         Exit;
       end
      else if fCurrPoint = 0 then
       begin
         fArcObject.Points[0] := CurrPoint2D;
         fArcObject.Points[1] := CurrPoint2D;
         fArcObject.Points[2] := CurrPoint2D;
         fArcObject.Points[3] := CurrPoint2D;
       end
      else if fCurrPoint = 2 then
       begin
         fArcObject.StartAngle := 0;
         fArcObject.EndAngle := 0;
         Description := 'Select the start and end angle of the arc.'
       end;
      fArcObject.Points[fCurrPoint] := CurrPoint2D;
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint2D := fArcObject.WorldToObject(CurrentViewportSnappedPoint);
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
      fArcObject.Points[fCurrPoint] := CurrPoint2D;
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
    end;
    cePaint: Viewport2D.DrawObject2DWithRubber(fArcObject, True);
   end;
end;

procedure TCAD2DDrawArcPrimitive.OnStop;
begin
  TCAD2DDrawArcPrimitiveParam(Param).fArcObject.Free;
  Param.Free;
  Param := nil;
end;

{ ******************* Editing tasks *********************** }

constructor TCAD2DSelectObjectsParam.Create(ApertureSize: Word; const AfterS: TCADStateClass);
begin
  inherited Create(AfterS);

  fApertureSize := ApertureSize;
  fSelectedObjs := TGraphicObjList.Create;
  fSelectedObjs.FreeOnClear := False;
  fSelectionFilter := TObject2D;
  fEndWithMouseDown := False;
  fEndIfNoObject := False;
end;

destructor TCAD2DSelectObjectsParam.Destroy;
begin
  fSelectedObjs.Free;
  inherited;
end;

procedure TCAD2DSelectObjectsParam.DrawOSD(Viewport: TCADViewport2D; const Pt: TPoint2D);
var
  ScrPt: TPoint;
begin
  with Viewport do
   begin
     ScrPt := Point2DToPoint(ViewportToScreen(Pt));
     OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
     OnScreenCanvas.Canvas.Pen.Style := psSolid;
     OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
   end;
end;

constructor TCAD2DSelectObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DSelectObject: Invalid param');
  Description := 'Use the mouse to select an object.';
  with TCAD2DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DSelectObject.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if fEndWithMouseDown and (MouseButton = cmbLeft) then begin
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) then
       begin
         fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
           if Assigned(fOnSelected) then
             fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
           Viewport2D.Refresh;
         finally
           IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
         Exit;
       end;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) then
       begin
         fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
           if Assigned(fOnSelected) then
             fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
           Viewport2D.Refresh;
         finally
           IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
       end;
    end;
    ceMouseMove: begin
      DrawOSD(Viewport2D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport2D, fLastPt);
    end;
    cePaint: DrawOSD(Viewport2D, fLastPt);
   end;
end;

procedure TCAD2DSelectObject.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD2DSelectObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DSelectObjects: Invalid param');
  Description := 'Use the mouse to select objects.';
  with TCAD2DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DSelectObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         IgnoreEvents := True;
         try
          Viewport2D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
            NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
          end;
         Result := True;
       end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D, fLastPt);
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         Removed := False;
         TmpExIter := fSelectedObjs.GetExclusiveIterator;
         try
           if TmpExIter.Search(TmpObj.ID) <> nil then
            begin
              TmpExIter.RemoveCurrent;
              Removed := True;
            end;
         finally
           TmpExIter.Free;
         end;
         if not Removed then
          fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
          if Assigned(fOnSelected) then
           fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, not Removed);
         finally
          IgnoreEvents := False;
         end;
       end;
      DrawOSD(Viewport2D, fLastPt);
    end;
    ceMouseMove: begin
      DrawOSD(Viewport2D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport2D, fLastPt);
    end;
    cePaint: begin
      DrawOSD(Viewport2D, fLastPt);
      if Assigned(fOnSelected) then
       begin
         IgnoreEvents := True;
         TmpIter := fSelectedObjs.GetIterator;
         try
           TmpIter.First;
           while TmpIter.Current <> nil do
            begin
              fOnSelected(TCAD2DSelectObjectsParam(Param), TObject2D(TmpIter.Current), PICK_NOOBJECT, True);
              TmpIter.Next;
            end;
         finally
          TmpIter.Free;
          IgnoreEvents := False;
         end;
       end;
    end;
   end;
end;

procedure TCAD2DSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DExtendedSelectObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DExtendedSelectObjects: Invalid param');
  Description := 'Use the mouse to select one object, hold shift key pressed to select more than one object.';
  with TCAD2DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DExtendedSelectObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                             var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         IgnoreEvents := True;
         try
          Viewport2D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
            NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
          end;
         Result := True;
       end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D, fLastPt);
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         Removed := False;
         TmpExIter := fSelectedObjs.GetExclusiveIterator;
         try
           if TmpExIter.Search(TmpObj.ID) <> nil then
            begin
              TmpExIter.RemoveCurrent;
              Removed := True;
            end;
         finally
           TmpExIter.Free;
         end;
         if not Removed then
          fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
          if Assigned(fOnSelected) then
           fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, not Removed);
         finally
          IgnoreEvents := False;
         end;
         // Check if the mouse button is pressed
         if Key <> VK_SHIFT then
          begin // No, then it behaves like selecting a single object.
            if Removed then
             fSelectedObjs.Add(TmpObj);
            IgnoreEvents := True;
            try
              if Assigned(fOnSelected) then
               fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
              Viewport2D.Refresh;
            finally
             IgnoreEvents := False;
            end;
            if Assigned(AfterState) then
             NextState := AfterState
            else
             begin
               Param.Free;
               Param := nil;
               NextState := DefaultState;
             end;
            Result := True;
            Exit;
          end;
       end;
      DrawOSD(Viewport2D, fLastPt);
    end;
    ceMouseMove: begin
      DrawOSD(Viewport2D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport2D, fLastPt);
    end;
    cePaint: begin
      DrawOSD(Viewport2D, fLastPt);
      if Assigned(fOnSelected) then
       begin
         IgnoreEvents := True;
         TmpIter := fSelectedObjs.GetExclusiveIterator;
         try
           TmpIter.First;
           while TmpIter.Current <> nil do
            begin
              fOnSelected(TCAD2DSelectObjectsParam(Param), TObject2D(TmpIter.Current), PICK_NOOBJECT, True);
              TmpIter.Next;
            end;
         finally
          TmpIter.Free;
          IgnoreEvents := False;
         end;
       end;
    end;
   end;
end;

procedure TCAD2DExtendedSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;



constructor TCAD2DSelectObjectsInAreaParam.Create(AreaMode: TGroupMode; const AfterS: TCADStateClass);
begin
  inherited Create(0, AfterS);

  fAreaMode := AreaMode;
end;

constructor TCAD2DSelectObjectsInArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCADPrgParam;
  LastFilt: TObject2DClass;
begin
  inherited;
  if StateParam is TCADPrgSelectAreaParam then
   begin // Return from Select area.
     NewParam := TCADPrgSelectAreaParam(Param).CallerParam;
     TCADPrgSelectAreaParam(Param).CallerParam := nil;
     TCAD2DSelectObjectsInAreaParam(NewParam).fArea := TCADPrgSelectAreaParam(StateParam).Area;
     Param.Free;
     Param := NewParam; // Set the parameter back to the original.
     with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsInAreaParam do
      begin
        LastFilt := Viewport2D.PickFilter;
        try
          Viewport2D.PickFilter := fSelectionFilter;
          Viewport2D.GroupObjects(fSelectedObjs, fArea, fAreaMode, False);
        finally
          Viewport2D.PickFilter := LastFilt;
        end;
        if Assigned(fOnSelected) then
         fOnSelected(TCAD2DSelectObjectsParam(Param), nil, PICK_NOOBJECT, True);
        IgnoreEvents := True;
        try
          Viewport2D.Refresh;
        finally
         IgnoreEvents := False;
        end;
        if Assigned(AfterState) then
         NextState := AfterState
        else
         begin
           Param.Free;
           Param := nil;
           NextState := DefaultState;
         end;
      end;
   end
  else if not (StateParam is TCAD2DSelectObjectsInAreaParam) then
   Raise ECADSysException.Create('TCAD2DSelectObjectsInArea: Invalid param')
  else
   begin
     NewParam := TCADPrgSelectAreaParam.Create(TCAD2DSelectObjectsInArea, Param);
     Param := NewParam;  // Set the parameter to the select area param.
     NextState := TCADPrgSelectArea;
   end;
end;

procedure TCAD2DSelectObjectsInArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ---------------------------- }

procedure TCAD2DTransformObjectsParam.TransformObjs(CurrPt: TPoint2D; UseOrto: Boolean);
begin
  if UseOrto then
   MakeOrto2D(fBasePt, CurrPt);
  fCurrTransf := GetTransform(fBasePt, CurrPt);
end;

procedure TCAD2DTransformObjectsParam.DrawWithFrame(Viewport: TCADViewport2D);
begin
  with Viewport do
   begin
     OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
     DrawBoundingBox2D(OnScreenCanvas, fBox,
                       RectToRect2D(OnScreenCanvas.Canvas.ClipRect),
                       MultiplyTransform2D(fCurrTransf, ViewportToScreenTransform));
   end;
end;

procedure TCAD2DTransformObjectsParam.DrawWithoutFrame(Viewport: TCADViewport2D);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjs.GetIterator;
  with Viewport do
   try
     TmpObj := TmpIter.First as TObject2D;
     while TmpObj <> nil do
      begin
        TmpObj.ModelTransform := fCurrTransf;
        DrawObject2DWithRubber(TmpObj, False);
        TmpObj := TmpIter.Next as TObject2D;
      end;
   finally
     TmpIter.Free;
   end;
end;

procedure TCAD2DTransformObjectsParam.DrawOSD(Viewport: TCADViewport2D);
begin
  Viewport.OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
  if fUseFrame then
   DrawWithFrame(Viewport)
  else
   DrawWithoutFrame(Viewport);
end;

procedure TCAD2DTransformObjectsParam.ConfirmTransform;
var
  TmpObj: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       TmpObj.ModelTransform := fCurrTransf;
       TmpObj.ApplyTransform;
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

procedure TCAD2DTransformObjectsParam.CancelTransform;
var
  TmpObj: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       TmpObj.ModelTransform := IdentityTransf2D;
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

constructor TCAD2DTransformObjectsParam.Create(Objs: TGraphicObjList; const AfterS: TCADStateClass);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  inherited Create(AfterS);

  fObjs := TGraphicObjList.Create;
  fObjs.FreeOnClear := False;
  fCurrTransf := IdentityTransf2D;
  if Objs.Count = 0 then
   Raise ECADSysException.Create('TCAD2DTransformObjectsParam: Invalid list');
  // Recupera il BBox a conferma la trasformazione Obj corrente.
  fUseFrame := Objs.Count > 1;
  TmpIter := Objs.GetIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    fBox := TmpObj.Box;
    while TmpObj <> nil do
     begin
       fObjs.Add(TmpObj);
       TmpObj.ApplyTransform;
       fBox := BoxOutBox2D(fBox, TmpObj.Box);
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

destructor TCAD2DTransformObjectsParam.Destroy;
begin
  fObjs.Free;
  inherited;
end;


function TCAD2DMoveObjectsParam.GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
begin
  Result := Translate2D(CurrPt.X - BasePt.X, CurrPt.Y - BasePt.Y);
end;

function TCAD2DRotateObjectsParam.GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
var
  A: TRealType;
begin
  A := ArcTan2(CurrPt.Y - BasePt.Y, CurrPt.X - BasePt.X);
  Result := Translate2D(-BasePt.X, -BasePt.Y);
  Result := MultiplyTransform2D(Result, Rotate2D(A));
  Result := MultiplyTransform2D(Result, Translate2D(BasePt.X, BasePt.Y));
end;


constructor TCAD2DTransformObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DTransformObjectsParam) then
   Raise ECADSysException.Create('TCAD2DTransformObjects: Invalid param');
  Description := 'Select the base point for the transformation.';
  with TCAD2DTransformObjectsParam(StateParam) do
   DrawWithFrame(TCADPrg2D(CADPrg).Viewport2D);
end;

function TCAD2DTransformObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DTransformObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         CancelTransform;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         ConfirmTransform;
         IgnoreEvents := True;
         try
          Viewport2D.Repaint;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      CurrPoint2D := CurrentViewportSnappedPoint;
      if fNPoint = 0 then
       begin
         fBasePt := CurrPoint2D;
         Description := 'Move the mouse to modify the transformation and press the mouse to apply it.';
       end
      else
       begin
         ConfirmTransform;
         IgnoreEvents := True;
         try
          Viewport2D.Repaint;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         Param.Free;
         Param := nil;
         Result := True;
         NextState := DefaultState;
         Exit;
       end;
      DrawOSD(Viewport2D);
      Inc(fNPoint);
    end;
    ceMouseMove: if fNPoint > 0 then begin
      DrawOSD(Viewport2D);
      CurrPoint2D := CurrentViewportSnappedPoint;
      TransformObjs(CurrPoint2D, UseOrto);
      DrawOSD(Viewport2D);
    end;
    cePaint: begin
      DrawOSD(Viewport2D);
    end;
   end;
end;

procedure TCAD2DTransformObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DMoveSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD2DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DMoveSelectedObjects: Invalid param');
  TmpList := TCAD2DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD2DMoveObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DMoveSelectedObjects;
  NextState := TCAD2DTransformObjects;
end;

constructor TCAD2DRotateSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD2DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DRotateSelectedObjects: Invalid param');
  TmpList := TCAD2DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD2DRotateObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DRotateSelectedObjects;
  NextState := TCAD2DTransformObjects;
end;

constructor TCAD2DRotateExSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpStr: String; TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param), TCADCmp2D(CADPrg.Viewport.CADCmp) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      if  TmpIter.Count = 0 then
      begin
        NextState := CADPrg.DefaultState;
        Param.Free;
        Param := nil;
        Exit;
      end;
      if (not InputQuery('Rotate', 'Angle', TmpStr) ) then
       begin
         NextState := CADPrg.DefaultState;
         Param.Free;
         Param := nil;
         Exit;
       end;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TObject2D(TmpIter.Current).Angle := StrToFloat(TmpStr);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      CADPrg.RepaintAfterOperation;
    end;
  NextState := CADPrg.DefaultState;
  Param.Free;
  Param := nil;
end;

constructor TCAD2DEditPrimitiveParam.Create(Prim: TPrimitive2D; ApertureSize: Word);
begin
  inherited Create(nil);

  fOriginalPrimitive := Prim;
  fCurrentPrimitive := CADSysFindClassByName(fOriginalPrimitive.ClassName).Create(0) as TPrimitive2D;
  if not Assigned(fCurrentPrimitive) then
   Raise ECADSysException.Create('TCAD2DEditPrimitive: Only registered classes are allowed');
  fCurrentPrimitive.Assign(fOriginalPrimitive);
  fApertureSize := ApertureSize;
  fCurrentCtrlPt := -1;
end;

destructor TCAD2DEditPrimitiveParam.Destroy;
begin
  fCurrentPrimitive.Free;
  inherited;
end;

procedure TCAD2DEditPrimitiveParam.SetCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
var
  TmpDist: TRealType;
  TmpAp: TRealType;
begin
  with Viewport do
   begin
     TmpAp := GetAperture(fApertureSize);
     fCurrentCtrlPt := fCurrentPrimitive.OnMe(Pt, TmpAp, TmpDist);
   end;
end;

procedure TCAD2DEditPrimitiveParam.AddCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
var
  TmpCPt: TPoint2D;
begin
  if fCurrentCtrlPt > -1 then
   begin
     DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
     TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, Pt);
     fCurrentPrimitive.Points.Insert(fCurrentCtrlPt, TmpCPt);
     DrawModifiedPrim(Viewport);
   end;
end;

procedure TCAD2DEditPrimitiveParam.UnSetCtrlPoint;
begin
  fCurrentCtrlPt := -1;
end;

procedure TCAD2DEditPrimitiveParam.AcceptEdited;
begin
  fOriginalPrimitive.Assign(fCurrentPrimitive);
end;

procedure TCAD2DEditPrimitiveParam.MoveCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
var
  TmpCPt: TPoint2D;
begin
  if fCurrentCtrlPt > -1 then
   begin
     DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
     TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, Pt);
     fCurrentPrimitive.Points[fCurrentCtrlPt] := TmpCPt;
     DrawModifiedPrim(Viewport);
   end;
end;

procedure TCAD2DEditPrimitiveParam.DrawOSD(Viewport: TCADViewport2D; Pt: TPoint2D; FirstTime: Boolean);
var
  ScrPt: TPoint;
begin
  with Viewport do
   begin
     OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
     if not FirstTime then
      begin
        ScrPt := Point2DToPoint(ViewportToScreen(fLastPt));
        OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                        Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                        Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                        Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                        Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
      end;
     fLastPt := Pt;
     ScrPt := Point2DToPoint(ViewportToScreen(fLastPt));
     OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
   end;
end;

procedure TCAD2DEditPrimitiveParam.DrawModifiedPrim(Viewport: TCADViewport2D);
begin
  with Viewport do
   DrawObject2DWithRubber(fCurrentPrimitive, True);
end;

constructor TCAD2DEditPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD2DEditPrimitiveParam;
begin
  inherited;
  if not (StateParam is TCADPrgParam) or not (TCADPrgParam(StateParam).UserObject is TPrimitive2D) then
   Raise ECADSysException.Create('TCAD2DEditPrimitive: Invalid param');
  Description := 'Select a Control point of the primitive';
  NewParam := TCAD2DEditPrimitiveParam.Create(TPrimitive2D(TCADPrgParam(StateParam).UserObject), 5);
  with TCADPrg2D(CADPrg) do
   begin
     Viewport2D.Refresh;
     NewParam.DrawOSD(Viewport2D, Point2D(0, 0), True);
     NewParam.DrawModifiedPrim(Viewport2D);
   end;
  Param := NewParam;
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DEditPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DEditPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         AcceptEdited;
         IgnoreEvents := True;
         try
          Viewport2D.Repaint;
         finally
          IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      SetCtrlPoint(Viewport2D, CurrentViewportPoint);
      if CurrentCtrlPt >= 0 then
       Description := 'Move the control point and release the mouse.';
    end;
    ceMouseDblClick: if (MouseButton = cmbLeft) and (fCurrentPrimitive.Points.GrowingEnabled) then begin
      SetCtrlPoint(Viewport2D, CurrentViewportPoint);
      if CurrentCtrlPt >= 0 then
       begin
         CurrPoint2D := CurrentViewportSnappedPoint;
         AddCtrlPoint(Viewport2D, CurrPoint2D);
       end;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      UnSetCtrlPoint;
      Description := 'Select a Control point of the primitive';
    end;
    ceMouseMove: begin
      CurrPoint2D := CurrentViewportSnappedPoint;
      DrawOSD(Viewport2D, CurrentViewportPoint, False);
      MoveCtrlPoint(Viewport2D, CurrPoint2D);
      DrawOSD(Viewport2D, CurrentViewportPoint, False);
    end;
    cePaint: begin
      DrawOSD(Viewport2D, CurrentViewportPoint, True);
      DrawModifiedPrim(Viewport2D);
    end;
   end;
end;

procedure TCAD2DEditPrimitive.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD2DEditSelectedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCADPrgParam;
  NewObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DEditSelectedObjects: Invalid param');
  with TCAD2DSelectObjectsParam(StateParam) do
   begin
     TmpIter := SelectedObjects.GetIterator;
     try
      NewObj := TmpIter.First as TObject2D;
     finally
      TmpIter.Free;
     end;
     if not (NewObj is TPrimitive2D) then
      Raise ECADSysException.Create('TCAD2DEditSelectedObjects: Invalid param');
     NewParam := TCADPrgParam.Create(nil);
     NewParam.UserObject := NewObj;
   end;
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DEditSelectedPrimitive;
  NextState := TCAD2DEditPrimitive;
end;

constructor TCAD2DCreateSourceBlock.Create(const ACADPrg: TCADPrg; const AStateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  TmpStr1: string;
  TmpIter: TExclusiveGraphicObjIterator;
  TmpBlk: TSourceBlock2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param), TCADCmp2D(CADPrg.Viewport.CADCmp) do
    begin
      TmpStr1 := 'New Sourceblock';
      if not InputQuery('Define block', 'Name', TmpStr1) then
       begin
         NextState := CADPrg.DefaultState;
         Param.Free;
         Param := nil;
         exit;
       end;

       TmpBlk := TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.FindSourceBlock(TmpStr1);
       if  (TmpBlk <> nil) then
       begin
         MessageDlg('Sorceblock ' + TmpStr1 + ' exists!' , mtError, [mbCancel], 0);
         NextState := CADPrg.DefaultState;
         Param.Free;
         Param := nil;
         exit;
       end;

      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           RemoveObject(Tmpiter.Current.ID);
           TmpIter.Next;
         end;
        TmpBlk := BlockObjects(StringToBlockName(TmpStr1), TmpIter);
        if Assigned(TmpBlk) then
          TmpBlk.IsLibraryBlock := True;
      finally
        TmpIter.Free;
      end;
      CADPrg.RepaintAfterOperation;
    end;
  NextState := CADPrg.DefaultState;
  Param.Free;
  Param := nil;
end;

constructor TCAD2DDeleteObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DMirrorXObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; TmpBottom: TRealType;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TmpBottom := TPrimitive2D(TmpIter.Current).Bottom;
           TPrimitive2D(TmpIter.Current).Transform(MirrorX2D);
           TPrimitive2D(TmpIter.Current).Bottom := TmpBottom;
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DMirrorYObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; TmpLeft: TRealType;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TmpLeft := TPrimitive2D(TmpIter.Current).Left;
           TObject2D(TmpIter.Current).Transform(MirrorY2D);
           TPrimitive2D(TmpIter.Current).Left := TmpLeft; //?????
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

procedure ScaleObject(AObject2D: TObject2D; AFactorX, AFactorY: TRealType);
var TmpTransf2D: TTransf2D;  TmpBottom: TRealType;    TmpLeft: TRealType;
begin
  TmpBottom := AObject2D.Bottom;
  TmpLeft := AObject2D.Left;
  TmpTransf2D := Scale2D(AFactorX, AFactorY);
  AObject2D.Transform(TmpTransf2D);
  AObject2D.Bottom := TmpBottom;
  AObject2D.Left   :=  TmpLeft;
end;

constructor TCAD2DScaleObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  TmpStrX, TmpStrY: String;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param), TCADCmp2D(CADPrg.Viewport.CADCmp) do
    begin
      if (not InputQuery('Scale', 'FactorX', TmpStrX)) or (not InputQuery('Scale', 'FactorY', TmpStrY)) then
       begin
         NextState := CADPrg.DefaultState;
         Param.Free;
         Param := nil;
         Exit;
       end;
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           ScaleObject(TObject2D(Tmpiter.Current), StrToFloat(TmpStrX), StrToFloat(TmpStrY));
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      CADPrg.RepaintAfterOperation;
    end;
  NextState := CADPrg.DefaultState;
  Param.Free;
  Param := nil;
end;

procedure ExplodeJustifiedVectText2D(JustifiedVectText2D: TJustifiedVectText2D);
begin

end;

procedure ExplodePolyline2DTPolygon2D(AOutline2D: TOutline2D);
var i: integer; TmpLine2D: TLine2D;
    P0, P1: TPoint2D;
begin
  AOutline2D.BeginUseProfilePoints;
  for i := 0   to AOutline2D.ProfilePoints.Count - 2 do
  begin
    TmpLine2D := TLine2D.Create(-1, AOutline2D.ProfilePoints[i], AOutline2D.ProfilePoints[i+1]);
    {TmpLine2D.Color      := AOutline2D.Color;
    TmpLine2D.PenStyle   := AOutline2D.PenStyle;
    TmpLine2D.PenWidth   := AOutline2D.PenWidth;
    TmpLine2D.ArrowStyle := AOutline2D.ArrowStyle;
    TmpLine2D.ReserveInt2:= AOutline2D.ReserveInt2;
    TmpLine2D.ReserveStr1:= AOutline2D.ReserveStr1;
    TmpLine2D.ReserveStr2:= AOutline2D.ReserveStr2;
    }
    TmpLine2D.Transform(AOutline2D.ModelTransform);
    TCADCmp2D(AOutline2D.OwnerCAD).AddObject(TmpLine2D.ID, TmpLine2D);
    TmpLine2D.fReserveInt1:= AOutline2D.fReserveInt1;
    TmpLine2D.LayerName  := AOutline2D.LayerName;
  end;
  if  (AOutline2D is TPolygon2D) then
  begin
    P0 := AOutline2D.ProfilePoints[AOutline2D.ProfilePoints.Count - 1];
    P1 := AOutline2D.ProfilePoints[0];
    TmpLine2D := TLine2D.Create(-1, P0, P1);
    {TmpLine2D.Color    := AOutline2D.Color;
    TmpLine2D.PenStyle := AOutline2D.PenStyle;
    TmpLine2D.PenWidth := AOutline2D.PenWidth;
    TmpLine2D.ArrowStyle := AOutline2D.ArrowStyle;
    TmpLine2D.ReserveInt1:= AOutline2D.ReserveInt1;
    TmpLine2D.ReserveInt2:= AOutline2D.ReserveInt2;
    TmpLine2D.ReserveStr1:= AOutline2D.ReserveStr1;
    TmpLine2D.ReserveStr2:= AOutline2D.ReserveStr2;
    }
    TmpLine2D.Transform(AOutline2D.ModelTransform);
    TCADCmp2D(AOutline2D.OwnerCAD).AddObject(TmpLine2D.ID, TmpLine2D);
    TmpLine2D.fReserveInt1:= AOutline2D.fReserveInt1;
    TmpLine2D.LayerName  := AOutline2D.LayerName;

  end;
  AOutline2D.EndUseProfilePoints;
end;

procedure ExplodeOutline2D(AOutline2D: TOutline2D);
var i: integer; TmpLine2D: TLine2D; TmpPolyline2D: TPolyline2D;
    P0, P1: TPoint2D;
begin
  if  AOutline2D is TPolyline2D then
  begin
    ExplodePolyline2DTPolygon2D(AOutline2D);
    Exit;
  end;
  AOutline2D.BeginUseProfilePoints;
  TmpPolyline2D := TPolyline2D.Create(-1, []);
  for i := 0   to AOutline2D.ProfilePoints.Count - 1 do
    TmpPolyline2D.Points.Add(AOutline2D.ProfilePoints[i]);
  TmpPolyline2D.Transform(AOutline2D.ModelTransform);
  TCADCmp2D(AOutline2D.OwnerCAD).AddObject(-1, TmpPolyline2D);
  AOutline2D.EndUseProfilePoints;
end;

{procedure MovePointsSet(APointsSet: TPointsSet2D; DX, DY: TRealType);
var i, x: integer; TmpPoint2D: TPoint2D;  TmpPointsSet2D: TPointsSet2D;
begin
  x := APointsSet.Count;
  try
    TmpPointsSet2D := TPointsSet2D.Create(APointsSet.Capacity);
    for i := 0 to  x - 1 do
    begin
      TmpPoint2D.X := APointsSet.Points[i].X + DX;
      TmpPoint2D.Y := APointsSet.Points[i].Y + DY;
      TmpPoint2D.W := 1;
      TmpPointsSet2D.Add(TmpPoint2D);
    end;
    APointsSet.Clear;
    APointsSet.Copy(TmpPointsSet2D, 0, x-1);
  finally
    TmpPointsSet2D.Clear;
    TmpPointsSet2D.Free;
  end;
end;}

procedure MovePointsSet(APointsSet: TPointsSet2D; DX, DY: TRealType);
begin
  APointsSet.TransformPoints(Translate2D(DX, DY));
end;

procedure ExplodeContainer(AContainer2D: TContainer2D; ADestCAD: TCADCmp2D);
var TmpIter: TExclusiveGraphicObjIterator;
    TmpClass: TGraphicObjectClass;
    TmpObj: TGraphicObject;
begin
  TmpIter := AContainer2D.Objects.GetExclusiveIterator;
  TmpObj  := TmpIter.First;
  try
    repeat
      if (TmpIter.Current is TCircle2D) then
      begin
        TmpObj := TCircle2D.Create(TmpIter.Current.ID, Point2D(0, 0), 0);
        TCircle2D(TmpObj).Assign(TCircle2D(TmpIter.Current));
        ADestCAD.AddObject(-1, TCircle2D(TmpObj));
        exit;
      end;
      TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
      TmpObj := TmpClass.Create(TmpIter.Current.ID);
      if (TmpObj is TCircle2D) then

      TmpObj.Assign(TmpIter.Current);
      if (TmpObj is TPrimitive2D) then
      begin
        TPrimitive2D(TmpObj).Transform(AContainer2D.ModelTransform);
        ADestCAD.AddObject(-1, TObject2D(TmpObj));
      end else
      if (TmpObj is TContainer2D) then
      begin
        ExplodeContainer(TContainer2D(TmpObj), ADestCAD);
      end;
    until TmpIter.Next = nil;
  finally
    TmpIter.Free;
  end;
end;

procedure ExplodeBlock(ABlock: TBlock2D; ADestCAD: TCADCmp2D);
var TmpIter: TExclusiveGraphicObjIterator;
    TmpClass: TGraphicObjectClass; TmpObj: TGraphicObject;
begin
  TmpIter := ABlock.SourceBlock.Objects.GetExclusiveIterator;
  TmpObj  := TmpIter.First;
  try
    repeat
      TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
      TmpObj := TmpClass.Create(TmpIter.Current.ID);
      TmpObj.Assign(TmpIter.Current);
      if (TmpObj is TPrimitive2D) then
      begin
        TPrimitive2D(TmpObj).Transform(ABlock.ModelTransform);
        ADestCAD.AddObject(-1, TObject2D(TmpObj));
      end else
      if (TmpObj is TBlock2D) then
      begin
        ExplodeBlock(TBlock2D(TmpObj), ADestCAD);
      end;
    until TmpIter.Next = nil;
  finally
    TmpIter.Free;
  end;
end;

procedure ExplodeFrame2D(AObject2D: TObject2D; ADestCAD: TCADCmp2D);
var TmpPolygon2D: TPolygon2D;
begin
  TmpPolygon2D := TPolygon2D.Create(-1, []);
  TmpPolygon2D.Assign(AObject2D);
  TmpPolygon2D.ProfilePoints.Copy(TFrame2D(AObject2D).ProfilePoints, 0, TFrame2D(AObject2D).ProfilePoints.Count - 2);
  ADestCAD.AddObject(-1, TmpPolygon2D);
end;

procedure ExplodePolygon2D(AObject2D: TObject2D; ADestCAD: TCADCmp2D);
var TmpPolyline2D: TPolyline2D;
begin
  TmpPolyline2D := TPolyline2D.Create(-1, []);
  TmpPolyline2D.Assign(TPolygon2D(AObject2D));
  TmpPolyline2D.ProfilePoints.Copy(TPolygon2D(AObject2D).ProfilePoints, 0, TPolygon2D(AObject2D).ProfilePoints.Count - 1);
  TmpPolyline2D.ProfilePoints.Add(TPolygon2D(AObject2D).ProfilePoints[0]);
  //TmpPolyline2D.Points.Add(TPolygon2D(AObject2D).ProfilePoints[0]);
  TmpPolyline2D.UpdateExtension(nil);
  ADestCAD.AddObject(-1, TmpPolyline2D);
end;

procedure ExplodeSegment2D(AObject2D: TObject2D; ADestCAD: TCADCmp2D);
var TmpCircularArc2D: TCircularArc2D;  TmpLine2D: TLine2D;
begin
  with TSegment2D(AObject2D) do
   begin
     TmpCircularArc2D := TCircularArc2D.Create(-1, MiddlePoint, Radius, StartAngle, EndAngle);
     TmpCircularArc2D.Direction := Direction;
     ADestCAD.AddObject(-1, TmpCircularArc2D);
     TmpLine2D := TLine2D.Create(-1, TmpCircularArc2D.ProfilePoints[TmpCircularArc2D.ProfilePoints.Count -1], TmpCircularArc2D.MiddlePoint);
     ADestCAD.AddObject(-1, TmpLine2D);
     TmpLine2D := TLine2D.Create(-1, TmpCircularArc2D.MiddlePoint, TmpCircularArc2D.ProfilePoints[0]);
     ADestCAD.AddObject(-1, TmpLine2D);
   end;
end;

procedure ExplodeSector2D(AObject2D: TObject2D; ADestCAD: TCADCmp2D);
var TmpCircularArc2D: TCircularArc2D;  TmpLine2D: TLine2D;
begin
  with TSector2D(AObject2D) do
   begin
     TmpCircularArc2D := TCircularArc2D.Create(-1, MiddlePoint, Radius, StartAngle, EndAngle);
     TmpCircularArc2D.Direction := Direction;
     ADestCAD.AddObject(-1, TmpCircularArc2D);
     TmpLine2D := TLine2D.Create(-1, TmpCircularArc2D.ProfilePoints[TmpCircularArc2D.ProfilePoints.Count -1], TmpCircularArc2D.ProfilePoints[0]);
     ADestCAD.AddObject(-1, TmpLine2D);
   end;
end;

procedure Explode(AObject2D: TObject2D; ADestCAD: TCADCmp2D);
begin
  if  (AObject2D is TPolygon2D) then
    ExplodePolygon2D(AObject2D, ADestCAD)
  else if  (AObject2D is TFrame2D) then
    ExplodeFrame2D(AObject2D, ADestCAD)
  {
  else if (AObject2D is TSegment2D) then
    ExplodeSegment2D(AObject2D, ADestCAD)
  else if (AObject2D is TSector2D) then
    ExplodeSector2D(AObject2D, ADestCAD)
  }
  else if  (AObject2D is TOutline2D) then
    ExplodeOutline2D(TOutline2D(AObject2D))
  else if (AObject2D is TJustifiedVectText2D) then
    ExplodeJustifiedVectText2D(TJustifiedVectText2D(AObject2D))
  else if (AObject2D is TContainer2D) then
    ExplodeContainer(TContainer2D(AObject2D), ADestCAD)
  else if (AObject2D is TBlock2D) then
    //ExplodeContainer(TContainer2D(TBlock2D(AObject2D).SourceBlock), ADestCAD);
    ExplodeBlock(TBlock2D(AObject2D), ADestCAD);
end;

constructor TCAD2DExplodeObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           if not (TmpIter.Current is TLine2D) then
           begin
             TObject2D(TmpIter.Current).Explode(true);
             if (not (TObject2D(TmpIter.Current) is TBitmap2D)) and (not (TObject2D(TmpIter.Current) is TJustifiedVectText2D))
             then TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           end;
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

procedure ReverseOutLine2D(AOutLine2D: TOutLine2D);
var i: integer; TmpPointsSet2D:  TPointsSet2D; TmpPoint2D: TPoint2D;
begin
  {if AOutLine2D is TBSpline2D then
  begin
    if TBSpline2D(AOutLine2D).Direction = adClockwise then
      TBSpline2D(AOutLine2D).Direction := adCounterClockwise
    else
      TBSpline2D(AOutLine2D).Direction := adClockwise;
    exit;
  end; }

  TmpPointsSet2D := TPointsSet2D.Create(AOutLine2D.ProfilePoints.Count);
  try
    for i := AOutline2D.ProfilePoints.Count - 1 downto 0 do
      TmpPointsSet2D.Add(AOutline2D.ProfilePoints[i]);

    AOutline2D.ProfilePoints.Clear;
    AOutline2D.ProfilePoints.Copy(TmpPointsSet2D, 0, (TmpPointsSet2D.Count - 1));

    {TmpPoint2D :=  AOutline2D.Points[AOutline2D.Points.Count-1];
    AOutline2D.Points[AOutline2D.Points.Count - 1] :=  AOutline2D.Points[0];
    AOutline2D.Points[0] := TmpPoint2D; }
  finally
    //AOutline2D.UpdateExtension(AOutline2D);
    TmpPointsSet2D.Clear;
    TmpPointsSet2D.Free;
  end;
end;

procedure ReverseLine2D(ALine2D: TLine2D);
var P0, P1: TPoint2D;
begin
  P0 := ALine2D.Points.Points[0];
  P1 := ALine2D.Points.Points[1];
  ALine2D.Points.Clear;
  ALine2D.Points.Add(P1);
  ALine2D.Points.Add(P0);
end;

procedure ReverseEllipticalArc2D(var AArcD: TEllipticalArc2D);
begin
  if AArcD.Direction  = adCounterClockwise then
    AArcD.Direction := adClockwise
  else
    AArcD.Direction := adCounterClockwise;
end;

procedure ReverseCircularArc2D(var AArcD: TCircularArc2D);
begin
  if AArcD.Direction  = adCounterClockwise then
    AArcD.Direction := adClockwise
  else
    AArcD.Direction := adCounterClockwise;
end;

procedure  ReverseEllipse2D(Ellipse2D: TEllipse2D);
begin
  if Ellipse2D.Direction  = adClockwise then
    Ellipse2D.Direction  := adCounterClockwise
  else
    Ellipse2D.Direction  := adClockwise;
end;

procedure  ReverseCircle2D(Circle2D: TCircle2D);
begin
  if Circle2D.Direction  = adClockwise then
    Circle2D.Direction  := adCounterClockwise
  else
    Circle2D.Direction  := adClockwise;
end;

procedure ReverseFrame2D(var AFrame2D: TFrame2D);
begin
  if AFrame2D.Direction  = adCounterClockwise then
    AFrame2D.Direction := adClockwise
  else
    AFrame2D.Direction := adCounterClockwise;
end;

procedure InverseFrame2D(var AFrame2D: TFrame2D);
begin
  if AFrame2D.Direction  = adCounterClockwise then
    AFrame2D.Direction := adClockwise
  else
    AFrame2D.Direction := adCounterClockwise;
end;

procedure Reverse(APrimitive2D: TPrimitive2D);
begin
  if (APrimitive2D is TBSpline2D) then
    TBSpline2D(APrimitive2D).Reverse

  else if (APrimitive2D is TLine2D) then
    ReverseLine2D(TLine2D(APrimitive2D))
  else if  (APrimitive2D is TEllipticalArc2D) then
    ReverseEllipticalArc2D(TEllipticalArc2D(APrimitive2D))
  else if  (APrimitive2D is TCircularArc2D) then
    ReverseCircularArc2D(TCircularArc2D(APrimitive2D))

  else if  (APrimitive2D is TEllipse2D) then
    ReverseEllipse2D(TEllipse2D(APrimitive2D))

  else if  (APrimitive2D is TCircle2D) then
    ReverseCircle2D(TCircle2D(APrimitive2D))

  else if  (APrimitive2D is TFrame2D) then
    ReverseFrame2D(TFrame2D(APrimitive2D))


  else if (APrimitive2D is TOutLine2D) then
    ReverseOutLine2D(TOutLine2D(APrimitive2D));
  APrimitive2D.UpdateExtension(APrimitive2D);
  if APrimitive2D.OwnerCAD <> nil then
    TCADCmp2D(APrimitive2D.OwnerCAD).Viewports[0].Repaint;
end;

procedure InverseEllipticalArc2D(AArcD: TEllipticalArc2D);
var TmpSA, TmpEA: TRealType;
begin
  TmpSA := AArcD.EndAngle;
  TmpEA := AArcD.StartAngle;
  AArcD.EndAngle   := TmpEA;
  AArcD.StartAngle := TmpSA;
  if AArcD.Direction  = adCounterClockwise then
    AArcD.Direction := adClockwise
  else
    AArcD.Direction := adCounterClockwise;
end;

procedure InverseCircularArc2D(AArcD: TCircularArc2D);
var TmpSA, TmpEA: TRealType;
begin
  TmpSA := AArcD.EndAngle;
  TmpEA := AArcD.StartAngle;
  AArcD.EndAngle   := TmpEA;
  AArcD.StartAngle := TmpSA;
  if AArcD.Direction  = adCounterClockwise then
    AArcD.Direction := adClockwise
  else
    AArcD.Direction := adCounterClockwise;
end;

procedure  InverseEllipse2D(Ellipse2D: TEllipse2D);
begin
  if Ellipse2D.Direction  = adClockwise then
    Ellipse2D.Direction  := adCounterClockwise
  else
    Ellipse2D.Direction  := adClockwise;
end;

procedure  InverseCircle2D(Circle2D: TCircle2D);
begin
  if Circle2D.Direction  = adClockwise then
    Circle2D.Direction  := adCounterClockwise
  else
    Circle2D.Direction  := adClockwise;
end;

{procedure InverseCircularArc2D(AArcD: TCircularArc2D);
var TmpSA, TmpEA: TRealType;
begin
  // Vertausche die Start- und Endwinkel
  TmpSA := AArcD.EndAngle;
  TmpEA := AArcD.StartAngle;

  AArcD.EndAngle := TmpEA;
  AArcD.StartAngle := TmpSA;

  // Ändere die Richtung des Bogens
  if AArcD.Direction = CounterClockwise then
    AArcD.Direction := Clockwise
  else
    AArcD.Direction := CounterClockwise;

  // Normalisiere die Winkel, falls nötig
  if AArcD.StartAngle < 0 then
    AArcD.StartAngle := AArcD.StartAngle + 2 * Pi
  else if AArcD.StartAngle >= 2 * Pi then
    AArcD.StartAngle := AArcD.StartAngle - 2 * Pi;

  if AArcD.EndAngle < 0 then
    AArcD.EndAngle := AArcD.EndAngle + 2 * Pi
  else if AArcD.EndAngle >= 2 * Pi then
    AArcD.EndAngle := AArcD.EndAngle - 2 * Pi;
end;}

procedure Inverse(APrimitive2D: TPrimitive2D);
begin
  if (APrimitive2D is TLine2D) then
    ReverseLine2D(TLine2D(APrimitive2D))
  else if  (APrimitive2D is TEllipticalArc2D) then
    InverseEllipticalArc2D(TEllipticalArc2D(APrimitive2D))
  else if  (APrimitive2D is TCircularArc2D) then
    InverseCircularArc2D(TCircularArc2D(APrimitive2D))

  else if  (APrimitive2D is TEllipse2D) then
    InverseEllipse2D(TEllipse2D(APrimitive2D))

  else if  (APrimitive2D is TCircle2D) then
    InverseCircle2D(TCircle2D(APrimitive2D))

  else if  (APrimitive2D is TFrame2D) then
    InverseFrame2D(TFrame2D(APrimitive2D))

  else if (APrimitive2D is TOutLine2D) then
    ReverseOutLine2D(TOutLine2D(APrimitive2D));
  APrimitive2D.UpdateExtension(APrimitive2D);
  if APrimitive2D.OwnerCAD <> nil then
    TCADCmp2D(APrimitive2D.OwnerCAD).Viewports[0].Repaint;
end;

constructor TCAD2DInverse.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           if  (TmpIter.Current is TSimplePrimitive2D) then
             TSimplePrimitive2D(TmpIter.Current).Inverse;
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DReverse.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           //if  (TmpIter.Current is TSimplePrimitive2D) then
             //TSimplePrimitive2D(TmpIter.Current).Reverse;
           TPrimitive2D(TmpIter.Current).Reverse;
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DSendToBack.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;  TmpStrList: TStringList; i: integer;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
     TmpIter := SelectedObjects.GetExclusiveIterator;
     TmpStrList := TStringList.Create;
   try
     TmpIter.First;
     while TmpIter.Current <> nil do
     begin
       TmpStrList.Add(IntToStr(TmpIter.Current.ID));
       TmpIter.Next;
     end;
   finally
     TmpIter.Free;
   end;
   for i := 0 to TmpStrList.Count - 1 do
     TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.SendToBack(StrToInt(TmpStrList[i]));
  end;
  TmpStrList.Free;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DBringToFront.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;  TmpStrList: TStringList; i: integer;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
     TmpIter := SelectedObjects.GetExclusiveIterator;
     TmpStrList := TStringList.Create;
   try
     TmpIter.First;
     while TmpIter.Current <> nil do
     begin
       TmpStrList.Add(IntToStr(TmpIter.Current.ID));
       TmpIter.Next;
     end;
   finally
     TmpIter.Free;
   end;
   for i := 0 to TmpStrList.Count - 1 do
     TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.BringToFront(StrToInt(TmpStrList[i]));
  end;
  TmpStrList.Free;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DBringForward.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;  TmpStrList: TStringList; i: integer;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
     TmpIter := SelectedObjects.GetExclusiveIterator;
     TmpStrList := TStringList.Create;
   try
     TmpIter.First;
     while TmpIter.Current <> nil do
     begin
       TmpStrList.Add(IntToStr(TmpIter.Current.ID));
       TmpIter.Next;
     end;
   finally
     TmpIter.Free;
   end;
   for i := 0 to TmpStrList.Count - 1 do
     TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.BringForward(StrToInt(TmpStrList[i]));
  end;
  TmpStrList.Free;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DSendBackwards.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;  TmpStrList: TStringList; i: integer;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
     TmpIter := SelectedObjects.GetExclusiveIterator;
     TmpStrList := TStringList.Create;
   try
     TmpIter.First;
     while TmpIter.Current <> nil do
     begin
       TmpStrList.Add(IntToStr(TmpIter.Current.ID));
       TmpIter.Next;
     end;
   finally
     TmpIter.Free;
   end;
   for i := 0 to TmpStrList.Count - 1 do
     TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.SendBackwards(StrToInt(TmpStrList[i]));
  end;
  TmpStrList.Free;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DSwapObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;  TmpFirstID, TmpSecondID: LongInt; TmpOwnerCAD: TCADCmp;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
     TmpIter := SelectedObjects.GetExclusiveIterator;
     try
       if TmpIter.Count < 2 then
       begin
         Param.Free;
         Param := nil;
         NextState := CADPrg.DefaultState;
         exit;
       end;
       TmpFirstID  := TmpIter.First.ID;
       TmpSecondID := TmpIter.Next.ID;
       TmpOwnerCAD := TGraphicObject(TmpIter.First).OwnerCAD;
     finally
       TmpIter.Free;
       TmpOwnerCAD.SwapObjects(TmpFirstID, TmpSecondID);
     end;
  end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DAlignLeft.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; TmpLeft: TRealType;  TmpObj2D: TObject2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpObj2D := TObject2D(TmpIter.First);
        if (TmpObj2D <> nil) and (TmpObj2D is TPrimitive2D) then
          TmpLeft := TPrimitive2D(TmpObj2D).Left;
        while TmpIter.Current <> nil do
        begin
          TmpIter.Next;
          if (TmpIter.Current <> nil) and (TmpIter.Current is TPrimitive2D) then
            TPrimitive2D(TmpIter.Current).Left := TmpLeft;
        end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
   end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DAlignTop.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; TmpTop: TRealType;  TmpObj2D: TObject2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpObj2D := TObject2D(TmpIter.First);
        if (TmpObj2D <> nil) and (TmpObj2D is TPrimitive2D) then
          TmpTop := TPrimitive2D(TmpObj2D).Top;
        while TmpIter.Current <> nil do
        begin
          TmpIter.Next;
          if (TmpIter.Current <> nil) and (TmpIter.Current is TPrimitive2D) then
            TPrimitive2D(TmpIter.Current).Top := TmpTop;
        end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
   end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DAlignRight.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; TmpRight: TRealType;  TmpObj2D: TObject2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpObj2D := TObject2D(TmpIter.First);
        if (TmpObj2D <> nil) and (TmpObj2D is TPrimitive2D) then
          TmpRight := TPrimitive2D(TmpObj2D).Right;
        while TmpIter.Current <> nil do
        begin
          TmpIter.Next;
          if (TmpIter.Current <> nil) and (TmpIter.Current is TPrimitive2D) then
            TPrimitive2D(TmpIter.Current).Right := TmpRight;
        end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
   end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DAlignBottom.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; TmpBottom: TRealType;  TmpObj2D: TObject2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
   begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpObj2D := TObject2D(TmpIter.First);
        if (TmpObj2D <> nil) and (TmpObj2D is TPrimitive2D) then
          TmpBottom := TPrimitive2D(TmpObj2D).Bottom;
        while TmpIter.Current <> nil do
        begin
          TmpIter.Next;
          if (TmpIter.Current <> nil) and (TmpIter.Current is TPrimitive2D) then
            TPrimitive2D(TmpIter.Current).Bottom := TmpBottom;
        end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
   end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

procedure CopyToCADClipboard(AIterator: TGraphicObjIterator);
var  TmpClass: TGraphicObjectClass; TmpObj: TGraphicObject;
begin
  if (AIterator.Count = 0) then  exit;

  if CADClipboard2D = nil then
    CADClipboard2D := TContainer2D.Create(-1, [nil])
  else
    CADClipboard2D.Objects.Clear;
  repeat
    TmpClass := TGraphicObjectClass(AIterator.Current.ClassType);
    TmpObj := TmpClass.Create(AIterator.Current.ID);
    TmpObj.Assign(AIterator.Current);
    CADClipboard2D.Objects.Add(TmpObj);
  until AIterator.Next = nil;
end;

constructor TCAD2DCopyObjectsToCADClipboard.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
  begin
    with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      if  (TmpIter <> nil) and (TmpIter.Count > 0) then
      begin
        try
          CopyToCADClipboard(TmpIter);
        finally
          TmpIter.Free;
        end;
      end;
    end;
    TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    Param.Free;
    Param := nil;
    NextState := CADPrg.DefaultState;
  end;
end;

constructor TCAD2DCutObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator; i: integer;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
  begin
    with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      if  (TmpIter <> nil) and (TmpIter.Count > 0) then
        CopyToCADClipboard(TmpIter);
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
    end;
    TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    Param.Free;
    Param := nil;
    NextState := CADPrg.DefaultState;
  end;
end;

constructor TCAD2DPasteObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpContainer2D: TContainer2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
  begin
    TmpContainer2D := TContainer2D.Create(-1, [nil]);
    TmpContainer2D.Assign(CADClipboard2D);
    try
      TCADPrg2D(CADPrg).StartOperation(TCAD2DPositionObject, TCAD2DPositionObjectParam.Create(nil, TmpContainer2D));
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    finally
      Param.Free;
      Param := nil;
      NextState := CADPrg.DefaultState;
    end;
  end;
end;

procedure OffsetCircle2D(ACircle2D: TCircle2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
var TmpCircle2D: TCircle2D;  P0: TPoint2D;
begin
  TmpCircle2D := TCircle2D.Create(-1, P0, 0);
  TmpCircle2D.Assign(ACircle2D);
  TmpCircle2D.Radius := ACircle2D.Radius + AOffset;
  ADestCAD.AddObject(-1, TmpCircle2D);
end;

procedure OffsetFrame2D(AFrame2D: TFrame2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
var TmpFrame2D: TFrame2D; P0, P1: TPoint2D;
begin
  TmpFrame2D := TFrame2D.Create(-1, P0, P1);
  TmpFrame2D.Assign(AFrame2D);

  P0.X := AFrame2D.Points[0].X - AOffset;
  P0.Y := AFrame2D.Points[0].Y + AOffset;
  P0.W := 1;
  P1.X := AFrame2D.Points[1].X + AOffset;
  P1.Y := AFrame2D.Points[1].Y - AOffset;
  P1.W := 1;

  TmpFrame2D.Points[0] := P0;
  TmpFrame2D.Points[1] := P1;

  ADestCAD.AddObject(-1, TmpFrame2D);
end;

procedure OffsetEllipticalArc2D(AEllipticalArc2D: TEllipticalArc2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
var TmpEllipticalArc2D: TEllipticalArc2D; P0, P1: TPoint2D;
begin
  TmpEllipticalArc2D := TEllipticalArc2D.Create(-1, P0, P1, 0, 0);

  TmpEllipticalArc2D.Assign(AEllipticalArc2D);
  P0.X := AEllipticalArc2D.Points[0].X - AOffset;
  P0.Y := AEllipticalArc2D.Points[0].Y + AOffset;
  P0.W := 1;
  P1.X := AEllipticalArc2D.Points[1].X + AOffset;
  P1.Y := AEllipticalArc2D.Points[1].Y - AOffset;
  P1.W := 1;

  TmpEllipticalArc2D.Points.Points[0] := P0;
  TmpEllipticalArc2D.Points.Points[1] := P1;

  ADestCAD.AddObject(-1, TmpEllipticalArc2D);
end;

procedure OffsetArc2D(AArc2D: TCircularArc2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
var TmpArc2D: TCircularArc2D;  P0: TPoint2D;
begin
  TmpArc2D := TCircularArc2D.Create(-1, Point2D(0, 0), 0, 0, 0);
  TmpArc2D.Assign(AArc2D);
  TmpArc2D.Radius := AArc2D.Radius + AOffset;
  ADestCAD.AddObject(-1, TmpArc2D);
end;

procedure OffsetEllipse2D(AEllipse2D: TEllipse2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
var TmpEllipse2D: TEllipse2D; P0, P1: TPoint2D;
begin
  TmpEllipse2D := TEllipse2D.Create(-1, P0, P1);
  TmpEllipse2D.Assign(AEllipse2D);

  P0.X := AEllipse2D.Points[0].X - AOffset;
  P0.Y := AEllipse2D.Points[0].Y + AOffset;
  P0.W := 1;
  P1.X := AEllipse2D.Points[1].X + AOffset;
  P1.Y := AEllipse2D.Points[1].Y - AOffset;
  P1.W := 1;

  TmpEllipse2D.Points[0] := P0;
  TmpEllipse2D.Points[1] := P1;

  ADestCAD.AddObject(-1, TmpEllipse2D);
end;

procedure OffsetPolygon(APolygon2D: TPolygon2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
var TmpPolygon2D: TPolygon2D; TmpPointsSet2: TPointsSet2d;
begin
  TmpPolygon2D := TPolygon2D.Create(-1, []);
  TmpPolygon2D.Assign(APolygon2D);

  TmpPointsSet2 := OffsetPolygon2D(APolygon2D.ProfilePoints, AOffset);

  TmpPolygon2D.ProfilePoints.Clear;
  TmpPolygon2D.ProfilePoints.Copy(TmpPointsSet2, 0, TmpPointsSet2.Count - 1);

  TmpPointsSet2.Free;
  TmpPointsSet2 := nil;

  ADestCAD.AddObject(-1, TmpPolygon2D);
end;

  procedure OffsetObjects(AObject2D: TObject2D; ADestCAD: TCADCmp2D; AOffset: TRealType);
  begin
    if  (AObject2D is TCircle2D) then
    begin
      OffsetCircle2D(TCircle2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;
    if  (AObject2D is TFrame2D) then
    begin
      OffsetFrame2D(TFrame2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;
    if  (AObject2D is TFrame2D) then
    begin
      OffsetFrame2D(TFrame2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;
    if  (AObject2D is TEllipticalArc2D) then
    begin
      OffsetEllipticalArc2D(TEllipticalArc2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;
    if  (AObject2D is TCircularArc2D) then
    begin
      OffsetArc2D(TCircularArc2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;
    if  (AObject2D is TEllipse2D) then
    begin
      OffsetEllipse2D(TEllipse2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;
        if  (AObject2D is TPolygon2D) then
    begin
      OffsetPolygon(TPolygon2D(AObject2D), ADestCAD, AOffset) ;
      exit;
    end;


    {if  (AObject2D is TOutline2D) then
      ExplodeOutline2D(TOutline2D(AObject2D))
    else if (AObject2D is TJustifiedVectText2D) then
      ExplodeJustifiedVectText2D(TJustifiedVectText2D(AObject2D))
    else if (AObject2D is TContainer2D) then
      ExplodeContainer(TContainer2D(AObject2D), ADestCAD)
    else if (AObject2D is TBlock2D) then
       ExplodeContainer(TContainer2D(TBlock2D(AObject2D).SourceBlock), ADestCAD); }
  end;

  constructor TCAD2DOffsetObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
  var
    TmpStr: String;
    TmpIter: TExclusiveGraphicObjIterator;
  begin
    inherited;
    if Param is TCAD2DSelectObjectsParam then
     with TCAD2DSelectObjectsParam(Param), TCADCmp2D(CADPrg.Viewport.CADCmp) do
      begin
        if (not InputQuery('Offset', 'Value', TmpStr) ) then
         begin
           NextState := CADPrg.DefaultState;
           Param.Free;
           Param := nil;
           Exit;
         end;
        TmpIter := SelectedObjects.GetExclusiveIterator;
        try
          TmpIter.First;
          while TmpIter.Current <> nil do
           begin
             OffsetObjects(TObject2D(Tmpiter.Current), TCADCmp2D(CADPrg.Viewport.CADCmp), StrToFloat(TmpStr));
             TmpIter.Next;
           end;
        finally
          TmpIter.Free;
        end;
        CADPrg.RepaintAfterOperation;
      end;
    NextState := CADPrg.DefaultState;
    Param.Free;
    Param := nil;
  end;

//Clipper
constructor TCAD2DClipperUnionObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TGraphicObjIterator;
ClipperInterface: TClipperInterface;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetIterator;
      ClipperInterface := TClipperInterface.Create;
      ClipperInterface.SrcGraphicObjList := SelectedObjects;
      ClipperInterface.ClipperUnion;
      ClipperInterface.ExportToCAD(TCADPrg2D(CADPrg).Viewport2D.CADCmp2D);
      ClipperInterface.Free;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DClipperIntersectionObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TGraphicObjIterator;
ClipperInterface: TClipperInterface;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetIterator;
      ClipperInterface := TClipperInterface.Create;
      ClipperInterface.SrcGraphicObjList := SelectedObjects;
      ClipperInterface.ClipperIntersection;
      ClipperInterface.ExportToCAD(TCADPrg2D(CADPrg).Viewport2D.CADCmp2D);
      ClipperInterface.Free;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DClipperDifferenceObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TGraphicObjIterator;
ClipperInterface: TClipperInterface;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetIterator;
      ClipperInterface := TClipperInterface.Create;
      ClipperInterface.SrcGraphicObjList := SelectedObjects;
      ClipperInterface.ClipperDifference;
      ClipperInterface.ExportToCAD(TCADPrg2D(CADPrg).Viewport2D.CADCmp2D);
      ClipperInterface.Free;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DClipperXOrObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TGraphicObjIterator;
ClipperInterface: TClipperInterface;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetIterator;
      ClipperInterface := TClipperInterface.Create;
      ClipperInterface.SrcGraphicObjList := SelectedObjects;
      ClipperInterface.ClipperXor;
      ClipperInterface.ExportToCAD(TCADPrg2D(CADPrg).Viewport2D.CADCmp2D);
      ClipperInterface.Free;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

constructor TCAD2DClipperOffsetObjects.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TGraphicObjIterator;  TmpStr: String;
ClipperInterface: TClipperInterface;
begin
  inherited;
  if (not InputQuery('Offset', 'Delta', TmpStr)) then
  begin
    NextState := CADPrg.DefaultState;
    Param.Free;
    Param := nil;
    Exit;
  end;

  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetIterator;
      ClipperInterface := TClipperInterface.Create;
      ClipperInterface.SrcGraphicObjList := SelectedObjects;
      ClipperInterface.ClipperOffset(StrToFloatDef(TmpStr, 20));
      ClipperInterface.ExportToCAD(TCADPrg2D(CADPrg).Viewport2D.CADCmp2D);
      ClipperInterface.Free;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           //TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;
//end-clipper

constructor TCAD2DMakeContainer.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TGraphicObjIterator; TmpContainer: TContainer2D;
    TmpClass: TGraphicObjectClass;
    TmpObj: TGraphicObject;
begin
  inherited;
  TmpContainer := nil;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetIterator;
      if TmpIter.Count > 0 then
        TmpContainer := TContainer2D.Create(-1, [nil]);
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
        begin
           TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
           TmpObj := TmpClass.Create(-1);
           TmpObj.Assign(TmpIter.Current);
           TmpContainer.Objects.Add(TmpObj);
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
        end;
        if TmpContainer <> nil then
          TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.AddObject(-1, TmpContainer);
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;


//CAM
constructor TCAD2DSetKerftypes.Create(const ACADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param) do
    begin
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           TPrimitive2D(TmpIter).fReserveInt1 := 0;
           TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.DeleteObject(TmpIter.Current.ID);
           TmpIter.Next;
         end;
      finally
        TmpIter.Free;
      end;
      TCADPrg2D(CADPrg).Viewport2D.CADCmp2D.RepaintViewports;
    end;
  Param.Free;
  Param := nil;
  NextState := CADPrg.DefaultState;
end;

initialization

finalization

end.


