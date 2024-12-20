{: This help file explain all the entities classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Shapes unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.
}
unit CS4Shapes;
{$MODE DELPHI}

interface

uses SysUtils, Classes, Graphics,  LCLType, LCLIntf, FPCanvas,
     CADSys4, CS4BaseTypes;

type

  TSimplePrimitive2D  = class;
  TDirectionalCurve2D = class;
  TClosedCurve2D      = class;
  TFrame2D = class;
  TBSpline2D = class;
  TText2D = class;
  TBitmap2D = class;
  TJustifiedVectText2D = class;

  {: This type defines the type used to specify the name of
     a <I=font type face> (like Times New Roman).
  }
  TFaceName = string[LF_FACESIZE];
  {: This class encapsulates the interface for the GDI font of
     Windows as defined by <Code=TLOGFONT> structure.

     This font is used by the library for the <See Class=TText2D>
     shape class. The use of it is somewhat difficult in the
     context of a 2D or 3D drawing so it is better to use
     the vectorial text shape <See Class=TJustifiedVectText2D> and
     <See Class=TJustifiedVectText3D>.
  }


  TExtendedFont = class(TObject)
  private
    LogFont: TLOGFONT;
    FHandle: HFONT;
    FCanvas: TCanvas;
    procedure SetNewValue;
    procedure SetCanvas(Cnv: TCanvas);
    procedure SetHeight(Value: Word);
    function GetHeight: Word;
    procedure SetWidth(Value: Word);
    function GetWidth: Word;
    procedure SetEscapement(Value: Word);
    function GetEscapement: Word;
    procedure SetOrientation(Value: Word);
    function  GetOrientation: Word;
    procedure SetWeight(Value: Word);
    function GetWeight: Word;
    procedure SetItalic(Value: Byte);
    function GetItalic: Byte;
    procedure SetUnderline(Value: Byte);
    function GetUnderline: Byte;
    procedure SetStrikeOut(Value: Byte);
    function GetStrikeOut: Byte;
    procedure SetCharSet(Value: Byte);
    function GetCharSet: Byte;
    procedure SetOutPrecision(Value: Byte);
    function GetOutPrecision: Byte;
    procedure SetClipPrecision(Value: Byte);
    function GetClipPrecision: Byte;
    procedure SetQuality(Value: Byte);
    function GetQuality: Byte;
    procedure SetPicthAndFamily(Value: Byte);
    function GetPicthAndFamily: Byte;
    procedure SetFaceName(Value: TFaceName);
    function GetFaceName: TFaceName;
  public
    {: This is the constructor that creates an instance of a
       font.

       When a new font is created it is set using the
       <I=DEFAULT_GUI_FONT> as defined in Windows specifications.
    }
    constructor Create;
    {: This destructor frees the font informations.

       It also detaches the font form a <I=Canvas>, if the font is
       currently in use by it.
    }
    destructor Destroy; override;
    {: This method assign the font data by using another font
       class as a prototype.

       Parameters:

       <LI=<I=Obj> is the font being used as a prototype.>
    }
    procedure Assign(Obj: TExtendedFont);
    {: This method saves the font informations into a stream.

       Parameters:

       <LI=<I=Strm> is the stream on which save the font structure.>
    }
    procedure SaveToStream(Strm: TStream);
    {: This method retrieves the font informations from a stream.

       Parameters:

       <LI=<I=Strm> is the stream from which retrieve the font
        structure.>
    }
    procedure LoadFromStream(Strm: TStream);
    {: This property attaches the font to a Canvas.

       If you want to use the font on a Canvas you must use this
       property. After you have setted this propery, to detach
       the font from the Canvas assign <B=nil> to this property.
    }
    property Canvas: TCanvas read FCanvas write SetCanvas;
    {: This property contains the handle for the
       <Code=TLOGFONT> structure.
    }
    property Handle: HFONT read FHandle;
  published
    {: This property specifies the <I=lfHeight> field of <Code=TLOGFONT>.
    }
    property Height: Word read GetHeight write SetHeight;
    {: This property specifies the <I=lfWidth> field of <Code=TLOGFONT>.
    }
    property Width: Word read GetWidth write SetWidth;
    {: This property specifies the <I=lfEscapement> field of
       <Code=TLOGFONT>.
    }
    property Escapement: Word read GetEscapement write SetEscapement;

    {: This property specifies the <I=lfOrientation> field of
       <Code=TLOGFONT>.
    }
    property Orientation: Word  read GetOrientation write SetOrientation;

    {: This property specifies the <I=lfWeight> field of
       <Code=TLOGFONT>.
    }
    property Weight: Word read GetWeight write SetWeight;
    {: This property specifies the <I=lfItalic> field of
       <Code=TLOGFONT>.
    }
    property Italic: Byte read GetItalic write SetItalic;
    {: This property specifies the <I=lfUnderline> field of
       <Code=TLOGFONT>.
    }
    property Underline: Byte read GetUnderline write SetUnderline;
    {: This property specifies the <I=lfStrikeOut> field of
       <Code=TLOGFONT>.
    }
    property StrikeOut: Byte read GetStrikeOut write SetStrikeOut;
    {: This property specifies the <I=lfCharSet> field of
       <Code=TLOGFONT>.
    }
    property CharSet: Byte read GetCharSet write SetCharSet;
    {: This property specifies the <I=lfOutPrecision> field of
       <Code=TLOGFONT>.
    }
    property OutPrecision: Byte read GetOutPrecision write SetOutPrecision;
    {: This property specifies the <I=lfClipPrecision> field of
       <Code=TLOGFONT>.
    }
    property ClipPrecision: Byte read GetClipPrecision write SetClipPrecision;
    {: This property specifies the <I=lfQuality> field of
       <Code=TLOGFONT>.
    }
    property Quality: Byte read GetQuality write SetQuality;
    {: This property specifies the <I=lfPitchAndFamily> field of
       <Code=TLOGFONT>.
    }
    property PicthAndFamily: Byte read GetPicthAndFamily write SetPicthAndFamily;
    {: This property specify the <I=lfFaceName> field of
       <Code=TLOGFONT>.
    }
    property FaceName: TFaceName read GetFaceName write SetFaceName;
  end;

  {: This type defines the <I=saving mode> used by the <I=outline> primitive
     shapes.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     This type is used to specify beetwen the two modes:

     <LI=<I=stSpace> forces an <I=outline> to recompute the
      shape's points whenever it must be drawed.>
     <LI=<I=stTime> tell to the <I=outline> to store the
      shape's points and compute them only when the control
      points are changed.>
  }
  TPrimitiveSavingType = (stSpace, stTime);


  {: This is the class reference type for the
     <See Class=TPrimitive2D> shape class.
  }
  TPrimitive2DClass = class of TPrimitive2D;

  {: This handler can be used to modify a primitive by dragging its
     control points.

     See also <See Class=TObject2DHandler>.
  }
  TPrimitive2DHandler = class(TObject2DHandler)
  public
    procedure DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); override;
    function OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=2D primitive>.

     A primitive shape is an entity which shape is controlled
     by a set of <I=control points>. This class stores and
     handles this set of point allowing the developer to
     focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     See also <See Class=TOutline2D> and <See Class=TCurve2D>.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }

  //The PointValue will be changed
  TCoordPoint2DInsp = class
    fOwner: TGraphicObject; fPointCode: integer;
  public
    function  GetX: TRealType;
    function  GetY: TrealType;
    procedure SetX(AValue: TRealType);
    procedure SetY(AValue: TRealType);
  public
    constructor create(AOwner: TGraphicObject; APointCode: integer);
  published
    property X: TRealType read GetX write SetX;
    property Y: TRealType read GetY write SetY;
  end;

  //The OwnerObject  will be moved
  TCoordPoint2DMoveObjectInsp = class
    fOwner: TGraphicObject; fPointIndex: word;
  public
    function  GetX: TRealType;
    function  GetY: TrealType;
    procedure SetX(AValue: TRealType);
    procedure SetY(AValue: TRealType);
  public
    constructor create(AOwner: TGraphicObject; APointIndex: word);
  published
    property X: TRealType read GetX write SetX;
    property Y: TRealType read GetY write SetY;
  end;

  TCoordPoints2DInsp = class
    protected
      fOwner: TGraphicObject; fPointIndex: integer;
      fStartPoint, fEndPoint: TCoordPoint2DInsp;
    public
      constructor create(AOwner: TGraphicObject);
      destructor  destroy; override;
    published
      property StartPoint:  TCoordPoint2DInsp  read  fStartPoint  write fStartPoint;
      property EndPoint:    TCoordPoint2DInsp  read  fEndPoint    write fEndPoint;
  end;

  TArcAngles = class
    fOwner: TGraphicObject;
  protected
    function  GetStartAngle: TRealType;
    procedure SetStartAngle(AAngle: TRealType);
    function  GetEndAngle: TRealType;
    procedure SetEndAngle(AAngle: TRealType);
    function  GetArcAngle: TRealType;
    procedure SetArcAngle(AAngle: TRealType);
  public
    constructor create(AOwner: TGraphicObject);
  published
    property StartAngle: TRealType read  GetStartAngle write SetStartAngle;
    property EndAngle:   TRealType read  GetEndAngle   write SetEndAngle;
    property ArcAngle:   TRealType read  GetArcAngle   write SetArcAngle;
  end;

  TEllipticalArc2DInsp = class
    fOwner: TGraphicObject;
    fArcAngles: TArcAngles;
    function   GetDirection: TArcDirection;
    procedure  SetDirection(AArcDirection: TArcDirection);
    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(AValue: word);
  public
    constructor create(AOwner: TGraphicObject);
    destructor destroy; override;
  published
    property ArcAngles: TArcAngles    read  fArcAngles   write fArcAngles;
    property Direction: TArcDirection read  GetDirection write SetDirection;
    property CurvePrecision: word     read  GetCurvePrecision write SetCurvePrecision;
  end;

  TCircle2DInsp = class
  protected
    fOwner: TGraphicObject;
    fCenterPoint: TCoordPoint2DMoveObjectInsp;
    function  GetRadius: TrealType;
    procedure SetRadius(AValue: TRealType);
    function  GetStartAngle: TrealType;
    procedure SetStartAngle(AValue: TRealType);
    function  GetDirection: TArcDirection;
    procedure SetDirection(ADirection: TArcDirection);
    function  GetCurvePrecision: word;
    procedure SetCurvePrecision(AValue: word);
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property CenterPoint: TCoordPoint2DMoveObjectInsp  read  fCenterPoint   write fCenterPoint;
    property StartAngle:  TRealType      read   GetStartAngle     write SetStartAngle;
    property Radius:      TRealType      read   GetRadius         write SetRadius;
    property Direction:   TArcDirection  read   GetDirection      write SetDirection;
    property CurvePrecision: word        read   GetCurvePrecision write SetCurvePrecision;
  end;

  TCircularArc2DInsp = class
    fOwner: TGraphicObject;
    fCenterPoint: TCoordPoint2DMoveObjectInsp;
    fArcAngles: TArcAngles;
    function   GetRadius: TrealType;
    procedure  SetRadius(AValue: TRealType);
    function   GetDirection: TArcDirection;
    procedure  SetDirection(ADirection: TArcDirection);
    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(AValue: word);
   public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property CenterPoint:    TCoordPoint2DMoveObjectInsp read  fCenterPoint   write fCenterPoint;
    property Radius:         TRealType      read  GetRadius          write SetRadius;
    property Direction:      TArcDirection  read  GetDirection       write SetDirection;
    property ArcAngles:      TArcAngles     read  fArcAngles         write fArcAngles;
    property CurvePrecision: word           read  GetCurvePrecision  write SetCurvePrecision;
   end;

  TSegment2DInsp = class
    fOwner: TGraphicObject;
    fCenterPoint: TCoordPoint2DMoveObjectInsp;
    fArcAngles: TArcAngles;
    function  GetRadius: TrealType;
    procedure SetRadius(AValue: TRealType);
    function  GetStartAngle: TrealType;
    procedure SetStartAngle(AValue: TRealType);
    function  GetDirection: TArcDirection;
    procedure SetDirection(ADirection: TArcDirection);
    function  GetEndAngle: TrealType;
    procedure SetEndAngle(AValue: TRealType);
    function  GetCurvePrecision: word;
    procedure SetCurvePrecision(AValue: word);
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property CenterPoint: TCoordPoint2DMoveObjectInsp  read  fCenterPoint   write fCenterPoint;
    property Radius:         TRealType      read  GetRadius         write SetRadius;
    property Direction:      TArcDirection  read  GetDirection      write SetDirection;
    property CurvePrecision: word           read  GetCurvePrecision write SetCurvePrecision;
    property ArcAngles:      TArcAngles     read fArcAngles         write fArcAngles;
    //property EndAngle:    TRealType      read  GetEndAngle    write SetEndAngle;
    //property StartAngle:  TRealType      read  GetStartAngle  write SetStartAngle;
  end;

  TSector2DInsp = class
     fOwner: TGraphicObject;
     fCenterPoint: TCoordPoint2DMoveObjectInsp;
     fArcAngles: TArcAngles;
     function  GetRadius: TrealType;
     procedure SetRadius(AValue: TRealType);
     function  GetStartAngle: TrealType;
     procedure SetStartAngle(AValue: TRealType);
     function  GetDirection: TArcDirection;
     procedure SetDirection(ADirection: TArcDirection);
     function  GetEndAngle: TrealType;
     procedure SetEndAngle(AValue: TRealType);
     function  GetCurvePrecision: word;
     procedure SetCurvePrecision(AValue: word);
   public
     constructor create(AOwner: TGraphicObject);
     destructor  destroy; override;
   published
     property CenterPoint:    TCoordPoint2DMoveObjectInsp  read  fCenterPoint   write fCenterPoint;
     property Radius:         TRealType      read  GetRadius         write SetRadius;
     property Direction:      TArcDirection  read  GetDirection      write SetDirection;
     property CurvePrecision: word           read  GetCurvePrecision write SetCurvePrecision;
     property ArcAngles:      TArcAngles     read fArcAngles         write fArcAngles;
     //property EndAngle:    TRealType      read  GetEndAngle    write SetEndAngle;
     //property StartAngle:  TRealType      read  GetStartAngle  write SetStartAngle;
   end;


  TBrushInspClosedCurve2D = class
  private
    fOwner: TGraphicObject;
    function   GetBrushSource: TBrushSource;
    procedure  SetBrushSource(ABrushSource: TBrushSource);
    function   GetBrushColor:  TColor;
    procedure  SetBrushColor(ABrushColor: TColor);
    function   GetBrushStyle:  TBrushStyle;
    procedure  SetBrushStyle(ABrushStyle: TBrushStyle);
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property  BrushSource: TBrushSource  read GetBrushSource   write SetBrushSource;
    property  BrushColor:  TColor        read GetBrushColor    write SetBrushColor;
    property  BrushStyle:  TBrushStyle   read GetBrushStyle    write SetBrushStyle;
  end;

  TBrushInspClosedPolyline2D = class
  private
    fOwner: TGraphicObject;
    function   GetBrushSource: TBrushSource;
    procedure  SetBrushSource(ABrushSource: TBrushSource);
    function   GetBrushColor:  TColor;
    procedure  SetBrushColor(ABrushColor: TColor);
    function   GetBrushStyle:  TBrushStyle;
    procedure  SetBrushStyle(ABrushStyle: TBrushStyle);
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property  BrushSource: TBrushSource  read GetBrushSource   write SetBrushSource;
    property  BrushColor:  TColor        read GetBrushColor    write SetBrushColor;
    property  BrushStyle:  TBrushStyle   read GetBrushStyle    write SetBrushStyle;
  end;

  TPenInsp = class
  private
    fOwner: TGraphicObject;
    function   GetPenSource: TPenSource;
    procedure  SetPenSource(APenSource: TPenSource);
    function   GetPenColor:  TColor;
    procedure  SetPenColor(APenColor: TColor);
    function   GetPenStyle:  TPenStyle;
    procedure  SetPenStyle(APenStyle: TPenStyle);
    function   GetPenWidth: word;
    procedure  SetPenWidth(APenWidht: word);
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property  PenSource: TPenSource  read GetPenSource   write SetPenSource;
    property  PenColor:  TColor      read GetPenColor    write SetPenColor;
    property  PenStyle:  TPenStyle   read GetPenStyle    write SetPenStyle;
    property  PenWidth:  word        read GetPenWidth    write SetPenWidth;
  end;

  TPrim2DInsp = class
     fOwner: TGraphicObject;
     fPenInsp: TPenInsp;
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property Pen: TPenInsp read fPenInsp write fPenInsp;
  end;

  TClosedCurve2DInsp = class
     fOwner: TGraphicObject;
     fBrushInsp: TBrushInspClosedCurve2D;
     function  GetArea:  TRealType;
   public
     constructor create(AOwner: TGraphicObject);
     destructor  destroy; override;
   published
     property Area:  TRealType  read  GetArea;
     property Brush: TBrushInspClosedCurve2D read fBrushInsp  write fBrushInsp;
   end;

  TPrimitive2D = class(TObject2D)
  private
     fPoints: TPointsSet2D;

     function  GetPenSource: TPenSource;  override;
     procedure SetPenSource(APenSource: TPenSource); override;

     function  GetPenColor: TColor;  override;
     procedure SetPenColor(APenColor: TColor); override;

     function  GetPenStyle: TPenStyle;  override;
     procedure SetPenStyle(APenStyle: TPenStyle); override;

     function  GetPenWidth: word;  override;
     procedure SetPenWidth(AValue: word); override;

  protected
    fPrim2DInsp: TPrim2DInsp;
    fPenSource:  TPenSource;
    fPenColor:   TColor;
    fPenStyle:   TPenStyle;
    fPenWidth:   word;

    procedure _UpdateExtension; override;
    {: This method allows you to change the type of the set
       of points used to store the <I=control points> of the
       primitive.

       When the entity is created this method is called to
       create the set of <I=control points> that defines
       the shape of the entity.

       By default a <See Class=TPointsSet2D> instance is created
       to store a maximum of <I=Size> points.

       You may want to override this property to create
       a special set of points that is able to store more
       information than a simple point. For instance you can
       derive a new set from <See Class=TPointsSet2D> that for
       every point store the kind of the point. This is the
       first step to create a <I=path shape> that draw arc
       segment as well as straight lines.
    }
    function  CreateVect(const Size: Integer): TPointsSet2D; dynamic;

    function  GetStartPoint: TPoint2D;           virtual; abstract;
    function  GetStartPointX: TRealType;         virtual; abstract;
    function  GetStartPointY: TRealType;         virtual; abstract;
    procedure SetStartPointX(AValue:TRealType);  virtual; abstract;
    procedure SetStartPointY(AValue:TRealType);  virtual; abstract;

    function  GetEndPointX: TRealType;           virtual; abstract;
    function  GetEndPointY: TRealType;           virtual; abstract;
    function  GetEndPoint: TPoint2D;             virtual; abstract;
    procedure SetEndPointX(AValue:TRealType);    virtual; abstract;
    procedure SetEndPointY(AValue:TRealType);    virtual; abstract;

    //propertys
    property  StartPointX:  TRealType   read GetStartPointX  write   SetStartPointX;
    property  StartPointY:  TRealType   read GetStartPointY  write   SetStartPointY;

    property  EndPointX:    TRealType   read GetEndPointX    write    SetEndPointX;
    property  EndPointY:    TRealType   read GetEndPointY    write    SetEndPointY;

    property  MiddlePointX: TRealType   read GetMiddlePointX write  SetMiddlePointX;
    property  MiddlePointY: TRealType   read GetMiddlePointY write  SetMiddlePointY;

  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp2D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet2D@GrowingEnabled>
       of <See Property=TPrimitive2D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
    }
    constructor Create(ID: LongInt; NPts: Integer);
    destructor  Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;

    {: This property contains the set of <I=control points> used
       to define the shape of the entity.

       See the introduction of <See Class=TPrimitive2D> for details.
    }
    property  Points: TPointsSet2D read fPoints write fPoints;

    property  StartPoint:  TPoint2D  read  GetStartPoint;
    property  EndPoint:    TPoint2D  read  GetEndPoint;
    property  MiddlePoint: TPoint2D  read  GetMiddlePoint;

    property  PenSource: TPenSource  read GetPenSource write SetPenSource;
    property  PenColor:  TColor      read GetPenColor  write SetPenColor;
    property  PenStyle:  TPenStyle   read GetPenStyle  write SetPenStyle;
    property  PenWidth:  word        read GetPenWidth  write SetPenWidth;

    property  Primitive2D: TPrim2DInsp  read fPrim2DInsp write  fPrim2DInsp;
  published
    //property  Primitive2D: TPrim2DInsp  read fPrim2DInsp write  fPrim2DInsp;
  end;

  TSimplePrim2DInsp = class
     fOwner: TGraphicObject;
     fCoordPoints2DInsp: TCoordPoints2DInsp;
     function   GetShowDirection: boolean;
     procedure  SetShowDirection(AValue: boolean);
     function   GetObjectLength: TRealType;
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property CoordPoints: TCoordPoints2DInsp read fCoordPoints2DInsp write fCoordPoints2DInsp;
    property ShowDirection: boolean read GetShowDirection write SetShowDirection;
    property ObjectLength: TRealType read GetObjectLength;
  end;

  TLine2DInsp = class
     fOwner: TGraphicObject;
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
  end;

  TSimplePrimitive2D = class(TPrimitive2D)
  private
    fSimplePrim2DInsp: TSimplePrim2DInsp;

    fShowDirection: boolean;

  protected

  public

    function  GetLength: TRealType;

    function  GetStartPoint: TPoint2D;            override;
    function  GetStartPointX: TRealType;          override;
    function  GetStartPointY: TRealType;          override;
    procedure SetStartPointX(AValue:TRealType);   override;
    procedure SetStartPointY(AValue:TRealType);   override;

    function  GetEndPointX: TRealType;            override;
    function  GetEndPointY: TRealType;            override;
    function  GetEndPoint: TPoint2D;              override;
    procedure SetEndPointX(AValue:TRealType);     override;
    procedure SetEndPointY(AValue:TRealType);     override;

    procedure   Reverse; override; //added
    procedure   Inverse; override; //adedd
    procedure   Explode(ADeleteSource: boolean); override; //added
    procedure   InitializeAngle; override; //added
    constructor create(ID: LongInt; NPts: Integer);
    destructor  destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure   Assign(const Obj: TGraphicObject); override;
    procedure   SaveToStream(const Stream: TStream); override;

    property ShowDirection: boolean read fShowDirection write fShowDirection;
    property ObjectLength: TRealType read GetLength;
  published
    property Primitive2D;
    property SimplePrimitive2D: TSimplePrim2DInsp read fSimplePrim2DInsp write fSimplePrim2DInsp;
  end;

  {: This class defines a <I=2D outline>.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     For an <I=outline> the control points are the same as
     the <I=profile points>.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     See also <See Class=TCurve2D>.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  //TOutline2D = class(TPrimitive2D)    //original
  TOutline2D = class(TSimplePrimitive2D)
  private
    function  GetStartPoint: TPoint2D;          override;
    function  GetStartPointX: TRealType;        override;
    function  GetStartPointY: TRealType;        override;
    procedure SetStartPointX(AValue:TRealType); override;
    procedure SetStartPointY(AValue:TRealType); override;

    function  GetEndPointX: TRealType;           override;
    function  GetEndPointY: TRealType;           override;
    function  GetEndPoint: TPoint2D;             override;
    procedure SetEndPointX(AValue:TRealType);    override;
    procedure SetEndPointY(AValue:TRealType);    override;

  protected
    {: This method is called when the <I=profile points>
       are needed.

       It must returns the set of the <I=profile points>
       created by the class.

       <B=Warning>: You don't have to delete the set of points
       returned by the method.
    }
    function GetProfilePoints: TPointsSet2D; virtual; abstract;
    {: This method returns the number of <I=profile points>
       that is equal (for an <I=outline>) to the number
       of <I=control points>.
    }
    function GetNPts: Integer; virtual; abstract;
    {: This method returns <B=True> if the set of <I=profile points>
       is closed.

       The fact that the set is closed influences the way in which
       it is drawed and picked.
    }
    function GetHasBrush: Boolean; virtual;

  public

    {: This method initializes the profile points vector for using.

       You must call this method before any use of the methods that
       work on the set of <I=profile points>.

       It is better to call this method in a <Code=try-finally>
       block with <See Method=TOutline2D@EndUseProfilePoints>
       in the finally part.
    }
    procedure BeginUseProfilePoints; dynamic;
    {: This method finalizes the <I=profile points> when you
       finish to use them.

       You must call this method when you no longer need to use
       the set of <I=profile points>. This allow the library
       to eventually saves the memory used by the entity.

       It is better to call this method in a <Code=try-finally>
       block with this method in the finally part.

       <B=Note>: This method must be called after the
       <See Method=TOutline2D@BeginUseProfilePoints>.
    }
    procedure EndUseProfilePoints; dynamic;
    {: This is the set of <I=profile points> that is used to
       draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property ProfilePoints: TPointsSet2D read GetProfilePoints;
    {: This property contains the size of the set of
       <I=profile points> that is used to draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property NumberOfProfilePts: Integer read GetNPts;
    {: This property is <B=True> when the shape of the
       entity must be considere as closed.
    }
    property HasBrush: Boolean read GetHasBrush;

    property StartPointX;
    property StartPointY;
    property EndPointX;
    property EndPointY;
  published

  end;

  {: This class defines a 2D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine2D = class(TSimplePrimitive2D)
    fSimplePrim2DInsp: TSimplePrim2DInsp;
  private
  public
    {: This constructor creates a new 2D line segment.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the starting point of the segment.>
       <LI=<I=P2> is the ending point of the segment.>
    }
    procedure   InitializeAngle; override;
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor destroy; override;

    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    procedure SetLength(AValue: TRealType);

  published
    property SimplePrimitive2D: TSimplePrim2DInsp read fSimplePrim2DInsp write fSimplePrim2DInsp;
  end;

  {: This class defines a 2D curve.

     A <I=curve> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     A curve is a 2D polyline in which the points that define
     the shape (<I=profile points>)are not the same as the
     <I=control points> of the entity. In this case the control
     points only control the shape of the curve.

     For this class the <See property=TCurve2D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve2D@PopulateCurvePoints> method.

     See also <See Class=TOutline2D>.

     <B=Note>: The control points and the profile points are
     always in the object model coordinate system !
  }
  {TCurve2DInsp = class
    fOwner: TGraphicObject;
    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(AValue: word);
  public
     constructor create(AOwner: TGraphicObject);
     destructor  destroy; override;
  published
    property CurvePrecision: word read GetCurvePrecision write SetCurvePrecision;
  end;}

  TCurve2D = class(TOutline2D)
  private
    fSavingType: TPrimitiveSavingType;
    fCurvePrecision: Word;
    fCurvePoints: TPointsSet2D;
    fCountReference: Integer;

    procedure SetCurvePrecision(N: Word);
    procedure SetPrimitiveSavingType(S: TPrimitiveSavingType);
    procedure FreeCurvePoints;
  protected
    procedure _UpdateExtension; override;
    {: This method is called whenever the <I=profile points>
       must be computed.

       You must redefine this method to fill the set of
       <I=control points> by using the actual set of
       <I=control points>. In defining this method you have
       to call the inherited method passing it the
       right number of <I=profile points> used by the
       entity as the <I=N> parameter.

       In the method you may use the
       <See Property=TOutline2D@ProfilePoints> to add the
       points to the set of <I=profile points>.

       <B=Warning>: Don't call
       <See Method=TOutline2D@BeginUseProfilePoints> nor
       <See Method=TOutline2D@EndUseProfilePoints> in this method.
       Also don't access the <See Property=TOutline2D@NumberOfProfilePts>
       property but use the number of points that you have
       computed.
    }
    function PopulateCurvePoints(N: Word): TRect2D; dynamic;
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    procedure BeginUseProfilePoints; override;
    procedure EndUseProfilePoints; override;
    {: This property may be used in the
       <See Method=TCurve2D@PopulateCurvePoints> as a parameters
       to control the precision (ie the number of <I=profile points>)
       used to draw the curve profile.

       By default it is 50.
    }
    property CurvePrecision: Word read fCurvePrecision write SetCurvePrecision;
    {: This property specify the saving mode used by the
       curve.

       By default it is set to <I=stTime>.

       See also <See Type=TPrimitiveSavingType>.
    }
    property SavingType: TPrimitiveSavingType read fSavingType write SetPrimitiveSavingType;
  end;

  TDirectionalCurve2DInsp = class
    fOwner: TGraphicObject;
    function   GetDirection: TArcDirection;
    procedure  SetDirection(ADirection: TArcDirection);
  public
     constructor create(AOwner: TGraphicObject);
     destructor  destroy; override;
  published
    property Direction: TArcDirection read GetDirection write SetDirection;
  end;

  TDirectionalCurve2D = class(TCurve2D)
  private
    fDirectionalCurve2DInsp: TDirectionalCurve2DInsp;
    fDirection: TArcDirection;
    procedure SetDirection(ArcDirection: TArcDirection);
    procedure SetStartPointX(AValue:TRealType); override;
    procedure SetStartPointY(AValue:TRealType); override;
    procedure SetEndPointX(AValue:TRealType);   override;
    procedure SetEndPointY(AValue:TRealType);   override;
  protected
  public
    procedure   Reverse; override; //added
    procedure   Inverse; override; //added
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor  destroy; override;
    property    Direction: TArcDirection read fDirection write SetDirection;
  published
    //property  DirectionalCurve2D: TDirectionalCurve2DInsp read fDirectionalCurve2DInsp write fDirectionalCurve2DInsp;
  end;

  TClosedCurve2D = class(TDirectionalCurve2D)
  private
    fClosedCurve2DInsp: TClosedCurve2DInsp;
    fArea:   TRealType;
    function GetArea: TRealType;
  protected
    fBrushSource: TBrushSource;
    fBrushColor:  TColor;
    fBrushStyle:  TBrushStyle;
  public
    procedure   Explode(ADeleteSource: boolean); override;
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
    destructor  destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream);  override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;

    function  GetBrushSource: TBrushSource;  override;
    procedure SetBrushSource(ABrushSource: TBrushSource); override;
    function  GetBrushColor: TColor;  override;
    procedure SetBrushColor(ABrushColor: TColor); override;
    function  GetBrushStyle: TBrushStyle;  override;
    procedure SetBrushStyle(ABrushStyle: TBrushStyle); override;
    property Area: TRealType read  GetArea;

    property BrushSource: TBrushSource read GetBrushSource   write SetBrushSource;
    property BrushColor:  TColor       read GetBrushColor    write SetBrushColor;
    property BrushStyle:  TBrushStyle  read GetBrushStyle    write SetBrushStyle;

    property HasBrush: boolean read GetHasBrush;
  published
    property ClosedCurve2D: TClosedCurve2DInsp read  fClosedCurve2DInsp write fClosedCurve2DInsp;
  end;

  {: This class defines a 2D polyline.

     A polyline is obtained by connecting the <I=profile points> (in
     this case are the same as the <I=control points>) with straight
     line segments.
  }
  TPolyline2D = class(TOutline2D)
  protected
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    {: This constructor creates a new 2D polyline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the polyline. If you want to create a pointless polyline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive2D@Points>.>
    }
    procedure  Explode(ADeleteSource: boolean); override;
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor  destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  published
    property SimplePrimitive2D: TSimplePrim2DInsp read fSimplePrim2DInsp write fSimplePrim2DInsp;
  end;

  TClosedPolyline2DInsp = class
    fOwner: TGraphicObject;
    fBrushInsp: TBrushInspClosedPolyline2D;
    function  GetArea: TRealType;
    function  GetDirection: TArcDirection;
    procedure SetDirection(AArcDirection: TArcDirection);
  public
     constructor create(AOwner: TGraphicObject);
     destructor  destroy; override;
  published
    property Brush: TBrushInspClosedPolyline2D read fBrushInsp write fBrushInsp;
    property Direction: TArcDirection read GetDirection write SetDirection;
    property Area: TRealType read  GetArea;
  end;

  TClosedPolyline2D = class(TPolyline2D)  //polygon
  private
    fClosedPolyline2DInsp: TClosedPolyline2DInsp;

    fBrushSource: TBrushSource;
    fBrushColor: TColor;
    fBrushStyle: TBrushStyle;

    fArea:  TRealType;
    fDirection: TArcDirection;
    function GetArea: TRealType;
  protected
  public
    procedure  Explode(ADeleteSource: boolean); override;
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    destructor destroy; override;

    property Direction: TArcDirection read fDirection write fDirection;
    property Area: TRealType read GetArea;

    property BrushSource: TBrushSource read fBrushSource write fBrushSource;
    property BrushColor:  TColor       read fBrushColor  write fBrushColor;
    property BrushStyle:  TBrushStyle  read fBrushStyle  write fBrushStyle;
  published
    property ClosedPolyline2D: TClosedPolyline2DInsp read fClosedPolyline2DInsp write fClosedPolyline2DInsp;
  end;

  {: This class defines a 2D polygon.

     A polygon is obtained by connecting the <I=profile points> (
     in this case they are the same as the <I=profile points>)
     with straight segments and filling the shape with the current
     brush of the Canvas.
  }

  TPolygon2D = class(TClosedPolyline2D)
  private
  protected
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;


  TFrame2DInsp = class
    fOwner: TGraphicObject;
    fStartCorner: TFrameStartCorner;
    function   GeStartCorner: TFrameStartCorner;
    procedure  SetStartCorner(AValue: TFrameStartCorner);
    function   GetWidth: TRealType;
    procedure  SetWidth(AValue: TRealType);
    function   GetHeight: TRealType;
    procedure  SetHeight(AValue: TRealType);
    function   GetDirection: TArcDirection;
    procedure  SetDirection(ADirection: TArcDirection);
    function   GetChamfered: boolean;
    procedure  SetfChamfered(AValue: boolean);
    function   GetChamferValue: TRealType;
    procedure  SetChamferValue(AValue:  TRealType);
  public
    constructor create(AOwner: TGraphicObject);
    destructor  destroy; override;
  published
    property Direction:    TArcDirection     read GetDirection    write SetDirection;
    property Height:       TRealType         read GetHeight       write SetHeight;
    property Width:        TRealType         read GetWidth        write SetWidth;
    //property Chamfered:    boolean           read GetChamfered    write SetfChamfered;
    //property ChamferValue: TRealType         read GetChamferValue write SetChamferValue;
    property StartCorner:  TFrameStartCorner read GeStartCorner   write SetStartCorner;
  end;

  {: This class defines a 2D rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle.
  }
  TFrame2D = class(TClosedCurve2D)
  private
    fFrame2DInsp: TFrame2DInsp;
    fStartCorner: TFrameStartCorner;
    fChamfered: boolean;
    fChamferValue: TRealType;
    fWidth,
    fHeight: TRealType;
    procedure  SetStartCorner(AValue: TFrameStartCorner);
    procedure  SetChamfered(AValue: boolean);
    procedure  SetChamferValue(AValue:  TRealType);

    function   GetWidth: TRealType;
    procedure  SetWidth(AValue: TRealType);
    function   GetHeight: TRealType;
    procedure  SetHeight(AValue: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D frame.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the bottom-left corner of the frame.>
       <LI=<I=P2> is the upper-right corner of the frame.>
    }
    procedure   InitializeAngle; override;

    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    destructor  destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    //procedure SaveToStream(const Stream: TStream; const Version: TCADVersion);  override;
    procedure SaveToStream(const Stream: TStream);  override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);

    property StartCorner:  TFrameStartCorner read fStartCorner  write SetStartCorner;
    property Chamfered:    boolean           read fChamfered    write SetChamfered;
    property ChamferValue: TRealType         read fChamferValue write SetChamferValue;
    property Width:        TRealType         read GetWidth        write SetWidth;
    property Height:       TRealType         read GetHeight       write SetHeight;
  published
    property Frame2D: TFrame2DInsp read fFrame2DInsp write fFrame2DInsp;
  end;

  {: This class defines a 2D filled rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle (these are in the object model space).
  }
  TRectangle2D = class(TFrame2D)
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines an arc segment of a 2D ellipse.

     The arc is defined by the two corner of the box that
     contains the arc's ellipse, and the starting and ending
     angles of the arc.
  }
  TEllipticalArc2D = class(TDirectionalCurve2D)
  private
    fEllipticalArc2DInsp: TEllipticalArc2DInsp;
    //FDirection:  TArcDirection;
    FStartAngle, FEndAngle: TRealType;
    function  GetStartAngle: TRealType;
    function  GetEndAngle: TRealType;
    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);

    function  GetArcAngle: TRealType;
    procedure SetArcAngle(A: TRealType);

    procedure GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor  creates a new arc of a 2D ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that defines the arc's ellipse.>
       <LI=<I=SA> is the starting angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>
       <LI=<I=SA> is the ending angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>

       Note: Once created, the arc has four control points. The
       first two are <I=P1> and <I=P2>; the third is the point
       that lies on the segment from the center of the arc's ellipse
       and the starting point of the arc; the fourth is the point
       that lies on the segment from the center of the arc's ellipse and
       the ending point of the arc.
    }

    procedure Inverse; override;
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
    destructor destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the starting angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property StartAngle: TRealType read GetStartAngle write SetStartAngle;
     {: This property contains the ending angle of the arc in radiants.

      The angle that correspond to zero radiants is along the
      positive x-axis (if no transformation is applied to the
      object).
     }
     property EndAngle: TRealType read GetEndAngle write SetEndAngle;
     property ArcAngle: TRealType read GetArcAngle write SetArcAngle;
  published
    property EllipticalArc2D: TEllipticalArc2DInsp read fEllipticalArc2DInsp write fEllipticalArc2DInsp;
  end;

  {: This class defines an arc segment of a 2D Circle.

     The arc is defined by the two corner of the box that
     contains the arc's ellipse, and the starting and ending
     angles of the arc.
  }
  TCircularArc2D = class(TDirectionalCurve2D)
  private
    fCircularArc2DInsp: TCircularArc2DInsp;
    fRadius: TrealType;
    FStartAngle, FEndAngle: TRealType;

    function  GetStartAngle: TrealType;
    function  GetEndAngle: TrealType;
    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);

    function  GetArcAngle: TRealType;
    procedure SetArcAngle(A: TRealType);

    procedure SetRadius(ARadius: TRealType);
    procedure GetArcParams(var CX, CY, R, SA, EA: TRealType);

    function  GetCenterPoint: TPoint2D;
    procedure SetCenterPoint(APoint2D: TPoint2D);
    function  GetCenterPointX: TRealType;
    procedure SetCenterPointX(AValue: TRealType);
    function  GetCenterPointY: TRealType;
    procedure SetCenterPointY(AValue: TRealType);

  protected
    procedure GetArcPoints(PP: TPointsSet2D; NPts: Word);
    function  PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor  creates a new arc of a 2D circle.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that defines the arc's ellipse.>
       <LI=<I=SA> is the starting angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>
       <LI=<I=SA> is the ending angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>

       Note: Once created, the arc has four control points. The
       first two are <I=P1> and <I=P2>; the third is the point
       that lies on the segment from the center of the arc's ellipse
       and the starting point of the arc; the fourth is the point
       that lies on the segment from the center of the arc's ellipse and
       the ending point of the arc.
    }
    procedure Inverse; override;
    constructor Create(ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
    destructor  destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;

    property CenterPoint2D:  TPoint2D  read  GetCenterPoint   write  SetCenterPoint;
    property CenterPoint2DX: TRealType read  GetCenterPointX  write  SetCenterPointX;
    property CenterPoint2DY: TRealType read  GetCenterPointY  write  SetCenterPointY;

    property Radius:       TRealType read fRadius         write SetRadius;
    property StartAngle:   TRealType read GetStartAngle   write SetStartAngle;
    property EndAngle:     TRealType read GetEndAngle     write SetEndAngle;
    property ArcAngle:     TRealType read GetArcAngle     write SetArcAngle;
  published

    property CircularArc2D: TCircularArc2DInsp read fCircularArc2DInsp write fCircularArc2DInsp;
   end;

  TCircle2D = class(TClosedCurve2D)
  private
    fCircle2DInsp: TCircle2DInsp;
    fRadius: TRealType;
    fStartAngle: TRealType;
    fCenterPoint: TPoint2D;
    procedure GetCircleParams(var CX, CY, R, SA: TRealType);
    function  GetStartAngle: TRealType;

    procedure SetStartAngle(AValue: TRealType);
    procedure SetRadius(AValue: TRealType);

    function  GetCenterPoint: TPoint2D;
    procedure SetCenterPoint(APoint2D: TPoint2D);
    function  GetCenterPointX: TRealType;
    procedure SetCenterPointX(AValue: TRealType);
    function  GetCenterPointY: TRealType;
    procedure SetCenterPointY(AValue: TRealType);

  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new circle.
       Parameters:
       P1 = Centrum pont
       P2 = Radius end point
    }
    constructor Create(ID: LongInt; const P1: TPoint2D; R : TRealType);
    destructor  destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;

    property Radius:     TRealType read  fRadius       write  SetRadius;
    property StartAngle: TRealType read  GetStartAngle write  SetStartAngle;

    property CenterPoint2D:  TPoint2D  read  GetCenterPoint   write  SetCenterPoint;
    property CenterPoint2DX: TRealType read  GetCenterPointX  write  SetCenterPointX;
    property CenterPoint2DY: TRealType read  GetCenterPointY  write  SetCenterPointY;

  published
    {: This property contains the starting angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property Circle2D: TCircle2DInsp read fCircle2DInsp write fCircle2DInsp;
  end;

  TSymetricSymbol2D = class(TCircle2D)
  public
    property EdgeCount: word read fCurvePrecision write  fCurvePrecision;
  end;

  TSegment2D = class(TClosedCurve2D)
  private
      fSegment2DInsp: TSegment2DInsp;
      fRadius: TrealType;
      FStartAngle, FEndAngle: TRealType;

      function  GetStartAngle: TRealType;
      function  GetEndAngle: TrealType;
      procedure SetStartAngle(A: TRealType);
      procedure SetEndAngle(A: TRealType);
      function  GetArcAngle: TRealType;
      procedure SetArcAngle(A: TRealType);
      procedure SetRadius(ARadius: TRealType);
      procedure GetArcParams(var CX, CY, R, SA, EA: TRealType);

    protected
      procedure   GetArcPoints(PP: TPointsSet2D; NPts: Word);
      function    PopulateCurvePoints(N: Word): TRect2D; override;
    public
      function  GetCenterPoint: TPoint2D;
      procedure SetCenterPoint(APoint2D: TPoint2D);
      function  GetCenterPointX: TRealType;
      procedure SetCenterPointX(AValue: TRealType);
      function  GetCenterPointY: TRealType;
      procedure SetCenterPointY(AValue: TRealType);

      procedure   Inverse; override;
      procedure   Explode(ADeleteSource: boolean); override;
      constructor Create(ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
      destructor  destroy; override;
      procedure   Assign(const Obj: TGraphicObject); override;
      constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
      procedure   SaveToStream(const Stream: TStream); override;

      property CenterPoint2D:  TPoint2D  read  GetCenterPoint   write  SetCenterPoint;
      property CenterPoint2DX: TRealType read  GetCenterPointX  write  SetCenterPointX;
      property CenterPoint2DY: TRealType read  GetCenterPointY  write  SetCenterPointY;

      property StartAngle: TRealType read GetStartAngle write SetStartAngle;
      property EndAngle:   TRealType read GetEndAngle   write SetEndAngle;
      property ArcAngle:   TRealType read GetArcAngle   write SetArcAngle;
      property Radius: TRealType read fRadius write  SetRadius;
    published
      property Segment2D: TSegment2DInsp read fSegment2DInsp write fSegment2DInsp;
    end;

  TSector2D = class(TClosedCurve2D)
  //TSector2D = class(TCircularArc2D)
  private
      fSector2DInsp: TSector2DInsp;
      fRadius: TrealType;
      FStartAngle, FEndAngle: TRealType;

      function  GetStartAngle: TRealType;
      function  GetEndAngle: TrealType;
      procedure SetStartAngle(A: TRealType);
      procedure SetEndAngle(A: TRealType);
      function  GetArcAngle: TRealType;
      procedure SetArcAngle(A: TRealType);

      procedure SetRadius(ARadius: TRealType);
      procedure GetArcParams(var CX, CY, R, SA, EA: TRealType);

    protected
      procedure GetArcPoints(PP: TPointsSet2D; NPts: Word);
      function  PopulateCurvePoints(N: Word): TRect2D; override;
    public
      function  GetCenterPoint: TPoint2D;
      procedure SetCenterPoint(APoint2D: TPoint2D);
      function  GetCenterPointX: TRealType;
      procedure SetCenterPointX(AValue: TRealType);
      function  GetCenterPointY: TRealType;
      procedure SetCenterPointY(AValue: TRealType);

      procedure   Inverse; override;
      procedure   Explode(ADeleteSource: boolean); override;
      constructor Create(ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
      destructor destroy; override;
      procedure Assign(const Obj: TGraphicObject); override;
      constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
      procedure SaveToStream(const Stream: TStream); override;

      property CenterPoint2D:  TPoint2D  read  GetCenterPoint   write  SetCenterPoint;
      property CenterPoint2DX: TRealType read  GetCenterPointX  write  SetCenterPointX;
      property CenterPoint2DY: TRealType read  GetCenterPointY  write  SetCenterPointY;

      property StartAngle: TRealType read GetStartAngle write SetStartAngle;
      property EndAngle:   TRealType read GetEndAngle   write SetEndAngle;
      property ArcAngle:   TRealType read GetArcAngle   write SetArcAngle;
      property Radius:     TRealType read fRadius write  SetRadius;
    published
      property  Sector2D: TSector2DInsp read fSector2DInsp write fSector2DInsp;
    end;

  {: This class defines a 2D ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }

  TEllipse2DInsp = class
    fOwner: TGraphicObject;
    function   GetWidth: TRealType;
    procedure  SetWidth(AValue: TRealType);
    function   GetHeight: TRealType;
    procedure  SetHeight(AValue: TRealType);
    function   GetDirection: TArcDirection;
    procedure  SetDirection(ADirection: TArcDirection);
    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(AValue: word);
  public
     constructor create(AOwner: TGraphicObject);
     destructor  destroy; override;
  published
    property Direction:      TArcDirection read GetDirection  write SetDirection;
    property Height:         TRealType     read GetHeight     write SetHeight;
    property Width:          TRealType     read GetWidth      write SetWidth;
    property CurvePrecision: word          read GetCurvePrecision write  SetCurvePrecision;
  end;


  TEllipse2D = class(TClosedCurve2D)
  private
    fEllipse2DInsp: TEllipse2DInsp;
    fWidth,
    fHeight: TRealType;
    function  GetWidth: TRealType;
    procedure SetWidth(AValue: TRealType);
    function  GetHeight: TRealType;
    procedure SetHeight(AValue: TRealType);

    procedure GetEllipseParams(var CX, CY, RX, RY: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that contains the ellipse.>
    }

    procedure   InitializeAngle;  override;

    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor  destroy; override;

    procedure Assign(const Obj: TGraphicObject); override;

    property Width: TRealType  read GetWidth write SetWidth;
    property Height: TRealType read GetHeight write SetHeight;
  published
    property Ellipse2D: TEllipse2DInsp  read fEllipse2DInsp  write fEllipse2DInsp;
  end;


    TASymetricSymbol2D = class(TEllipse2D)
    public
      property EdgeCount: word read fCurvePrecision write  fCurvePrecision;
    end;


  {: This class defines a 2D filled ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }
  TFilledEllipse2D = class(TEllipse2D)
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
    function OnMe(Pt: TPoint2D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D B-Spline curve.

     The B-Spline is defined by its control points.
     The order of the spline is 3 but you can change it.
  }
  //TBSpline2D = class(TCurve2D) //original
  TBSpline2D = class(TCurve2D)
  private
    fOrder: Byte;
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D spline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the control points
        of the spline. If you want to create a pointless spline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;

    {: This property contains the order of the spline.

       By default it is three (cubic spline).
    }
    property Order: Byte read FOrder write FOrder;
  published
    //property EndPointX;
    //property EndPointY;
     //property Coordinates;
  end;

  {: This class defines a 2D text object that uses the Windows
     font for drawing.

     I created a new text object that is more suitable for a
     2D/3D CAD programs. By the fact that any TTF may be
     converted into the new font format, I discourage the use
     of this object that is keept in the library only for
     backward compatibility.

     See <See Class=TVectFont> for information on the
     new type of Text object.

     <B=Note>: The new text object is not able to fill the
      interior of characters. If you need this capability you
      still have to use this kind of Text object.
  }

  TText2DInsp = class
  private
    fOwner: TText2D;
    function   GetText: TStringList;
    procedure  SetText(ATextList: TStringList);
    function   GetHeight: TRealType;
    procedure  SetHeight(AValue: TRealType);
    function   GetColorSource: TColorSource;
    procedure  SetColorSource(AColorSource: TColorSource);
    function   GetColor: TColor;
    procedure  SetColor(AColor: TColor);
    function   GetClippingFlags: integer;
    procedure  SetClippingFlags(AClippingFlags: integer);
    function   GetAutoSize: boolean;
    procedure  SetAutoSize(AValue: boolean);
    function   GetDrawBox: boolean;
    procedure  SetDrawBox(AValue: boolean);
    function   GetFaceName: TFaceName;
    procedure  SetFaceName(AFaceName: TFaceName);
    function   GetItalic: boolean;
    procedure  SetItalic(AValue: boolean);
    function   GetStrikeout: boolean;
    procedure  SetStrikeout(AValue: boolean);
    function   GetUnderline: boolean;
    procedure  SetUnderline(AValue: boolean);
    function   GetBold: boolean;
    procedure  SetBold(AValue: boolean);

    function  GetTextLines: TStringList;
    procedure SetTextLines(AStringList: TStringList);

  public
    constructor create(AOwner: TText2D);
    destructor  destroy; override;
  published
    //property Text:          AnsiString    read  GetText          write SetText;
    property Height:        TRealType     read  GetHeight        write SetHeight;
    property ColorSource:   TColorSource  read  GetColorSource   write SetColorSource;
    property Color:         TColor        read  GetColor         write SetColor;
    //property ClippingFlags: integer       read  GetClippingFlags write SetClippingFlags;
    property AutoSize:      boolean       read  GetAutoSize      write SetAutoSize;
    property DrawBox:       boolean       read  GetDrawBox       write SetDrawBox;
    property Italic:        boolean       read  GetItalic        write SetItalic;
    property Strikeout:     boolean       read  GetStrikeout     write SetStrikeout;
    property Underline:     boolean       read  GetUnderline     write SetUnderline;
    property Bold:          boolean       read  GetBold          write SetBold;
    property FaceName:      TFaceName     read  GetFaceName      write SetFaceName;

    property TextList: TStringList read GetTextLines write SetTextLines;
  end;

  TText2D = class(TPrimitive2D)
  private
    fText2DInsp: TText2DInsp;
    fTextList: TStringList;
    fText: AnsiString;
    fHeight: TRealType;
    fExtFont: TExtendedFont;
    fDrawBox, fRecalcBox: Boolean;
    fClippingFlags: Integer; // Win32s DrawText flags.
    //added
    fColorSource: TColorSource;
    fColor: TColor;
    fAutoSize,
    fItalic,
    fStrikeout,
    fUnderline,
    fBold: boolean;
    fFaceName: TFaceName;

    function   GetFaceName: TFaceName;
    procedure  SetFaceName(AFaceName: TFaceName);
    function   GetItalic: boolean;
    procedure  SetItalic(AValue: boolean);
    function   GetStrikeout: boolean;
    procedure  SetStrikeout(AValue: boolean);
    function   GetUnderline: boolean;
    procedure  SetUnderline(AValue: boolean);
    function   GetBold: boolean;
    procedure  SetBold(AValue: boolean);

    function  GetTextLines: TStringList;
    procedure SetTextLines(AStringList: TStringList);
  public
    {: Create a new text entity in the rectangle <I=Rect1>, with
       the given <I=Height> and <I=Text>.

       Points[0] will be the left-bottom corner of the text
       bounding box (also used as clipping box for the text)
       and Points[1] the right-up corner.

       The <I=width> of the text is set to the width of <I=Rect1>.
       If you don�t know the dimension of the Text on screen, set
       the <See Property=TText2D@AutoSize> property to <B=True>.
       The first time the text will be drawed the bounding box will
       be adjusted automatically.

       The rectangle will be drawed in the current brush and pen
       if <See Property=TText2D@DrawBox> property is <B=True>.
    }
    procedure   Reverse; override; //added
    procedure   Inverse; override; //adedd
    procedure   InitializeAngle; override;
    procedure   SetAngle(AAngle: TRealType); override;
    constructor Create(ID: LongInt; Rect1: TRect2D; Height: TRealType; Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor  Destroy; override;
    procedure   Assign(const Obj: TGraphicObject); override;
    procedure   SaveToStream(const Stream: TStream); override;
    procedure   Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function    OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the heigth of the text in world
       units.

       This is different from the Heigth of font used to render
       the text object.
    }
    property Height: TRealType read fHeight write fHeight;
    {: This property contains the <See Class=TExtendedFont>
       instance used to render the text.

       Use it to change the font.
    }
    property LogFont: TExtendedFont read FExtFont;
    {: If this property is set to <B=True> the text box is
       drawed below the text. Otherwise only the text is drawed.

       By default it is <B=False>.
    }
    property DrawBox: Boolean read FDrawBox write FDrawBox;
    {: If this property is <B=True>, the bounding box is changed
       when the object is drawed so it contains the whole text.

       By default it is <B=False>.
    }
    property AutoSize: Boolean read fRecalcBox write fRecalcBox;
    {: This property contains the text string used by the
       text entity.

       You may include more than one line of text simply
       adding <Code=#10#13> beetwen lines.
    }
    //property Text: AnsiString read FText write FText;
    {: This property contains the <I=clipping flags> used
       by drawing the text with the <I=DrawText> API function.

       By default the are setted to <I=DT_NOCLIP>.
    }
    property ClippingFlags: Integer read FClippingFlags write FClippingFlags;

    //added
    property ColorSource: TColorSource  read fColorSource   write fColorSource;
    property Color: TColor              read fColor         write fColor;
    property Italic: boolean            read GetItalic      write SetItalic;
    property Strikeout: boolean         read GetStrikeout   write SetStrikeout;
    property Underline: boolean         read GetUnderline   write SetUnderline;
    property Bold: boolean              read GetBold        write SetBold;
    property FaceName: TFaceName        read GetFaceName    write SetFaceName;

    property Text: AnsiString read fText write fText;
    property TextList: TStringList read GetTextLines write SetTextLines;
  published
    property Text2D: TText2DInsp read fText2DInsp write fText2DInsp;
  end;

  {: This class rapresents a scalable raster bitmap.

     This object is useful when you want a bitmap with world
     dimension that is scaled when you zoom in a portion of the
     drawing. For instance this can be useful for GIS
     applications. However this object isn�t fast and sometimes
     the bitmap is not drawed. Maybe the problem is the Windows
     95 bitmap support. I will add faster and reliable
     raster capability to the library as soon as I have time.

     You can however use a thirdy part library to enhance this
     object by using this class as a blueprint for your
     specific bitmap entity.
  }

  TBitmap2DInsp = class
    fOwner: TBitmap2D;
    function  GetScaleFactor: TRealType;
    procedure SetScaleFactor(SF: TRealType);
    function  GetAspectRatio: TRealType;
    procedure SetAspectRatio(AR: TRealType);
    function  GetCopyMode: TCopyMode;
    procedure SetCopyMode(ACopyMode: TCopyMode);
  public
    constructor create(AOwner: TBitmap2D);
    destructor  destroy; override;
  published
    property ScaleFactor: TRealType read GetScaleFactor write SetScaleFactor;
    property AspectRatio: TRealType read GetAspectRatio write SetAspectRatio;
    property CopyMode:    TCopyMode read GetCopyMode write SetCopyMode;
  end;

  TBitmap2D = class(TPrimitive2D)
  private
    fBitmap2DInsp: TBitmap2DInsp;
    fBitmap: TBitmap;
    fScaleFactor: TRealType;
    fAspectRatio: TRealType;
    fCopyMode: TCopyMode;
    procedure SetScaleFactor(SF: TRealType);
    procedure SetAspectRatio(AR: TRealType);
  public
    {: This constructor creates a new bitmap object.

       <I=Bmp> is the bitmap to be drawed and it will be freed
        by the object.

       <I=P1> and <I=P2> are the corner points of the bitmap
       in world coordinates (and the bitmap will be stretched
       to fit in).

       <B=Note>: The bitmap cannot be rotated !
    }
    procedure InitializeAngle; override;
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; Bmp: TBitmap);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    {: This property contains the bitmap to be drawed.

       It will be freed by the object.
    }
    property Bitmap: TBitmap read FBitmap;
    {: This property may contains the scale factor to be used for the
       bitmap.

       If you set this property the bounding box is recomputed and
       the bitmap is not stretched. The first Points will remain
       in the position specified but the second point will be
       repositioned using the ScaleFactor.

       The value rapresent how many drawing unit correspond to
       one pixel in the image. So an image of 50x50 pixels with
       a ScaleFactor of 2.0 will be 100x100 drawing units large.
       How many application units (cm, inch, etc) is a drawing
       unit is left to you.
       
       Setting it to zero (the default) means that no scale is
       needed.
    }
    property ScaleFactor: TRealType read fScaleFactor write SetScaleFactor;
    {: This property may contains the aspect ratio (Width/Heigth) to
       be used for the bitmap.

       This property is used only if ScaleFactor is not 0.0

       Setting it to zero (the default) means that no aspect ratio is
       needed.
    }
    property AspectRatio: TRealType read fAspectRatio write SetAspectRatio;
    {: This property contains the CopyMode used to copy the
       bitmap.
    }
    property CopyMode: TCopyMode read fCopyMode write fCopyMode;
  published
    property Bitmap2D: TBitmap2DInsp  read fBitmap2DInsp write fBitmap2DInsp;
  end;

  {: This class defines a 2D/3D vectorial char as a set of
     polylines.

     The number of polylines that can be used to define a char
     is limited by the size given at the moment of creation.

     In the char definition the polylines must be defined in the
     unitary square that ranges from X:0,1 and Y:0,1, so that
     the char may be scaled with a scale transformation.

     When the char is drawed inside a string, the real dimension
     of the char is used (the font is a proportional one).

     <B=Note>: The vectorial character may be created with
     conversion from Windows True Type Font with a
     separate utility given with the library.
  }
  TVectChar = class(TObject)
  private
    fSubVects: TIndexedObjectList;
    fExtension: TRect2D;

    function GetVect(Idx: Integer): TPointsSet2D;
    function GetVectCount: Integer;
  public
    {: This constructor creates an new instance of the vectorial char.

       Parameters:

       <LI=<I=NSubVect> is the number of polylines that defines
        the char.>
    }
    constructor Create(NSubVect: Integer);
    destructor Destroy; override;
    {: This method create a new instance of a char definition by
       retrieves its datas from a stream.

       Parameters:

       <LI=<I=Stream> is the stream that contains the char
        definition (previously saved with
        <See Method=TVectChar@SaveToStream>).>
    }
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion);
    {: This method saves a char definition into a stream.

       Parameters:

       <LI=<I=Stream> is the stream into which save the char
        definition (that can be retrived with
        <See Method=TVectCharCreateFromStream>).>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method is used computes the real dimension of the
       char.

       This method <B=MUST> be called when the definition of
       the char is finished (that is when you finish to
       create the polylines of the char).
    }
    procedure UpdateExtension(Sender: TObject);
    {: This property contains the set of polylines that defines
       the char.

       The polylines are stored in instances of
       <See Class=TPointsSet2D>.

       <I=Idx> is the index of the polyline in the set.
    }
    property Vectors[Idx: Integer]: TPointsSet2D read GetVect;
    {: This property contains the number of polylines that
       defines the char.

       This property cannot be changed.
    }
    property VectorCount: Integer read GetVectCount;
    {: This property contains the bounding box of the char.

       This value is updated by calling
       <See Method=TVectChar@UpdateExtension>. You must call
       that method before use this property.
    }
    property Extension: TRect2D read fExtension;
  end;

  {: This class defines a 2D vectorial font typeface.

     The typeface is made up of 256 instances of
     <See Class=TVectChar> (so no unicode may be used). The font
     may be stored and retrived from a font file.

     The easy way to define a font typeface is by using the
     <I=True Type Font> converter that create it from a TTF font.

     The chars are indexed by their <I=ASCII> value. If a char
     is not defined, it will not be drawed and an underline will
     be shown.

     The space and carriage returns are handled automatically,
     but you can change their shape.

     Any font used by the application is registered in the
     system with an <I=Index>, that is used to save and retrieve
     the font from the disk and associate the correct font to
     the texts.

     If you change the indexes among different drawings, you
     could load the wrong file. In this case you will notice that
     the text is drawed with a different font. If the index
     doesn't correspond to any font and no default font is
     defined an exception will be raised and the drawing will not
     be loaded.

     See <See Function=CADSysSetDefaultFont> and the other
     registration functions for details.
  }
  TVectFont = class(TObject)
  private
    fVects: TIndexedObjectList;

    function GetChar(Ch: Char): TVectChar;
  public
    constructor Create;
    destructor Destroy; override;
    {: This method creates an instance of the font and retrieves
       its definition from a Stream.

       Parameters:

       <LI=<I=Stream> is the stream from which retrieve the
        font definition.>
    }
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion);
    {: This method saves an instance of the font into a Stream.

       Parameters:

       <LI=<I=Stream> is the stream to which saves the font
        definition.>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method draws a char of the font on a canvas using a
       transform mapping from a 2D viewing system.

       Parameters:

       <LI=<I=Ch> is the index (ASCII) of the char to be drawed.>
       <LI=<I=DrawPoint> is the 2D point in world coordinates at
        which draw the char (it is the botton-left corner of the
        bounding box of the char). The point is changed by the
        method into the position for the next char on the same
        text line.>
       <LI=<I=H> is the size of the char in world units. This value
        is used to scale the char definition.>
       <LI=<I=ICS> is the space between two chars (in normalized
        units). For example a value of 0.2 means a space of
        20% of H.>
       <LI=<I=VT> is the mapping transform from world to screen.
        It may be obtained from the
        <See Property=TCADViewport@ViewportToScreenTransform>
        property.>
       <LI=<I=Cnv> is the canvas on which draw the char.>
    }
    procedure DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv: TDecorativeCanvas);
    {: This method returns the bounding box of a vectorial text string
       when the current font is used to draw it.

       Parameters:

       <LI=<I=Str> is the string.>
       <LI=<I=H> is the size of the char in world units.>
       <LI=<I=InterChar> is the space beetwen two chars
        (in normalized units). For example a value of 0.2 means
        a space of 20% of H.>
       <LI=<I=InterLine> is the space beetwen two lines of the
        text (in normalizaed units). For example a value of 0.2
        means a space of 20% of H.>
    }
    function GetTextExtension(Str: AnsiString; H, InterChar, InterLine: TRealType): TRect2D;
    {: This method creates a new char definition and returns the
       <See Class=TVectChar> that rapresents it.

       You must use the returned value to define the char.

       If the char is already present it will be deleted.

       Parameters:

       <LI=<I=Ch> is the characted to be defined.>
       <LI=<I=N> is the number of polylines used to draw the char.>
    }
    function CreateChar(Ch: Char; N: Integer): TVectChar;
    {: This property contains the set of chars of the font.

       The characters are indexed by their <I=ASCII> code.
    }
    property Chars[Ch: Char]: TVectChar read GetChar;
  end;

  {: This type defines the horizontal justification mode for a
     2D/3D vectorial text:

     <LI=<I=jhLeft> means left justification.>
     <LI=<I=jhRight> means right justification.>
     <LI=<I=jhCenter> means center justification.>
  }
  THJustification = (jhLeft, jhRight, jhCenter);
  {: This type defines the vertical justification mode for a
     2D/3D vectorial text:

     <LI=<I=jvTop> means top justification.>
     <LI=<I=jvBottom> means bottom justification.>
     <LI=<I=jvCenter> means center justification.>
  }
  TVJustification = (jvTop, jvBottom, jvCenter);


  TJustifiedVectText2DInsp = class
    fOwner: TJustifiedVectText2D;
    fBasePoint: TCoordPoint2DInsp;
    function  GetCharSpace: TRealType;
    procedure SetCharSpace(AValue: TRealType);
    function  GetDrawBox: boolean;
    procedure SetDrawBox(AValue: boolean);
    function  GetHeight: TRealType;
    procedure SetHeight(AValue: TRealType);
    function  GetHJustification: THJustification;
    procedure SetHJustification(AValue: THJustification);
    function  GetVJustification: TVJustification;
    procedure SetVJustification(AValue: TVJustification);
    function  GetInterline: TRealType;
    procedure SetInterline(AValue: TRealType);
    function  GetText: string;
    procedure SetText(AValue: string);
    function  GetFontIndex: word;
    procedure SetFontIndex(AValue: word);
  public
    constructor create(AOwner: TJustifiedVectText2D);
    destructor  destroy; override;
  published
    //property BasePoint: TCoordPoint2DInsp read fBasePoint write fBasePoint; ????
    property CharSpace: TRealType  read GetCharSpace write SetCharSpace;
    property DrawBox: boolean read GetDrawBox  write SetDrawBox;
    property Height: TRealType read GetHeight write SetHeight;
    property HJustification: THJustification read GetHJustification write SetHJustification;
    property VJustification: TVJustification read GetVJustification write SetVJustification;
    property Interline: TRealType read GetInterline write SetInterline;
    property Text: string read GetText write SetText;
    property FontIndex: word read  GetFontIndex write SetFontIndex;
  end;

  {: This class defines the 2D vectorial text.

     The text may be multilines and justified. It uses an
     instance of <See Class=TVectFont> to extract the typeface
     to be used.
  }
  TJustifiedVectText2D = class(TPrimitive2D)
  private
    fJustifiedVectText2DInsp: TJustifiedVectText2DInsp;
    fVectFont: TVectFont;
    fText: AnsiString;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fBasePoint: TPoint2D;
    fHeight, fCharSpace, fInterLine: TRealType;
    fDrawBox: Boolean;

    procedure SetHeight(H: TRealType);
    procedure SetCharSpace(S: TRealType);
    procedure SetInterLine(S: TRealType);
    procedure SetText(T: String);
    function GetTextExtension: TRect2D;
    procedure DrawText(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);

    //added
    function  GetFontIndex: word;
    procedure SetFontIndex(AValue: word);

  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is identifier that univocally identify the
        object in the CAD. By means of the method used to add the
        object to the CAD, the <I=ID> of the object might be different
        from the one supplied here. See <See Method=TCADCmp@AddObject>
        for details.>
       <LI=<I=FontVect> is the font typeface. Use
        <See Function=CADSysFindFontByIndex> and
        <See Function=CADSysFindFontIndex> for details.>
       <LI=<I=TextBox> is the rectangle used to justify the text.
        The string is drawed from the upper-left corner of this
        box.>
       <LI=<I=Height> is the size of the font in world units.>
       <LI=<I=Txt> is the text to be drawed.>
    }
    procedure InitializeAngle; override;
    constructor Create(ID: LongInt; FontVect: TVectFont; TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the size of the font in world unit.
    }
    property Height: TRealType read fHeight write SetHeight;
    {: This property contains the spacing beetwen chars in
       normalized unit.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property CharSpace: TRealType read fCharSpace write SetCharSpace;
    {: This property contains the spacing beetwen lines of the
       text.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property InterLine: TRealType read fInterLine write SetInterLine;
    {: This property contains the instance of the font that is
       used to draw the text.
    }
    property VectFont: TVectFont read fVectFont write fVectFont;
    {: If this property is <B=True>, a frame is drawed around
       the text.
    }
    property DrawBox: Boolean read fDrawBox write fDrawBox;
    {: This property contains the text to be drawed.
    }
    property Text: AnsiString read FText write SetText;
    {: This property specifies the horizontal justification.
    }
    property HorizontalJust: THJustification read fHJustification write fHJustification;
    {: This property specifies the vertical justification.
    }
    property VerticalJust: TVJustification read fVJustification write fVJustification;

    property BasePoint: TPoint2D  read fBasePoint write fBasePoint;

    property FontIndex: word read GetFontIndex write SetFontIndex;

  published
    property JustifiedVectText2D: TJustifiedVectText2DInsp read fJustifiedVectText2DInsp write fJustifiedVectText2DInsp;
  end;


  {: This procedure sets the default font.
  }
  procedure CADSysSetDefaultFont(const Font: TVectFont);
  {: This function returns the default font.
  }
  function  CADSysGetDefaultFont: TVectFont;
  {: This function initializes the list of registered fonts.
  }
  procedure CADSysInitFontList;
  {: This function clears the list of registered fonts.
  }
  procedure CADSysClearFontList;
  {: This function returns the index of a font.

     If the font is not registered an exception will be raised.

     Parameters:

     <LI=<I=Font> is the font to be searched.>
  }
  function  CADSysFindFontIndex(const Font: TVectFont): Word;
  {: This function returns the font that was registered with
     the specified index.

     If the index is not used and a default font is defined,
     the function returns the default font. Otherwise an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the index of the font to be searched for.>
  }
  function  CADSysFindFontByIndex(Index: Word): TVectFont;
  {: This function register a font by retriving it from
     a file.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=FileName> is name of the file that contains the
      font.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
  procedure CADSysRegisterFontFromFile(Index: Word; const FileName: String);
  {: This function register a font.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=Font> is the font to be registered.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
  procedure CADSysRegisterFont(Index: Word; const Font: TVectFont);
  {: This function clear the registration of a font.

     Parameters:

     <LI=<I=Index> is the registration index.>
  }
  procedure CADSysUnregisterFont(Index: Word);

const
  {: This constats rapresent the maximum number of vectorial fonts
     that may be used in the library.
  }
  MAX_REGISTERED_FONTS = 512;


type
  TArrowType = (atArrow, arThick);

TDimension2D = class(TPrimitive2D)
private
  FArrowlength: double;
  FArrowheight: Double;
  FArrowType: TArrowType;
public
 txt :TText2D;
 constructor Create(ID: LongInt; const P1, P2: TPoint2D);
 procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
 property ArrowLength :Double read FArrowlength write FArrowLength;
 property ArrowHeight :Double read FArrowheight write Farrowheight;
 property ArrowType :TArrowType read FArrowType write FArrowType;
end;


implementation

uses Math, Dialogs;

var
  VectFonts2DRegistered: array[0..MAX_REGISTERED_FONTS] of TVectFont;
  _NullChar: TVectChar;
  _DefaultFont: TVectFont;
  _DefaultHandler2D: TPrimitive2DHandler;




  { TDimension }
  constructor TDimension2D.Create(ID: Integer; const P1, P2: TPoint2D);
  begin
    inherited Create(ID, 2);
    Points.DisableEvents := True;
    try
      Points.Add(P1);
      Points.Add(P2);
      Points.GrowingEnabled := True;
    finally
      Points.DisableEvents := False;
      UpdateExtension(Self);
    end;
    txt := TText2D.Create(0,REct2D(0,0,0,0),5,'Temp String');
    txt.ClippingFlags := $100; //dt_left and dt_bottom;
    txt.Height := 20;
    //fSubObject:= 1;
    ArrowHeight := 2;
    ArrowLength:= 8;
    ArrowType:= atArrow;
  end;

  procedure TDimension2D.Draw(const VT: TTransf2D;  const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D;  const DrawMode: Integer);
  var
   pt1, pt2 :TPointsSet2D;
   p1:TPoint2D;
   testang, ang, angB, deltaX, deltaY, arr :Double;
   tmpRect2D:TRect2D;
   Rect :TRect;
  begin
    //inherited;
    Pt1 := TPointsSet2D.Create(3);
    Pt2 := TPointsSet2D.Create(3);
    if not HasTransform then
     DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, VT)
    else
     DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, MultiplyTransform2D(ModelTransform, VT));

    Pt1.Add(Points[0]); p1 := points[0];

    deltaX := Points[1].X-Points[0].X;   deltaY := Points[1].Y-POints[0].Y;
    if deltaX <> 0 then ang := arctan(deltaY/deltaX);
    if arrowLength <> 0 then AngB := Arctan( ArrowHeight/ArrowLength);
    arr := sqrt(power(arrowheight,2)+power(arrowlength,2));

    Pt1.Add(Point2D((p1.X+cos(ang-angB)*arr),(p1.Y+sin(ang-angB)*arr)));
    pt1.Add(Point2D((p1.X+cos(ang+angB)*arr),(p1.Y+sin(ang+angB)*arr)));
    Cnv.Canvas.Brush.Color := clBlue;
    Pt1.DrawAsPolygon(cnv,ClipRect2D,Box,VT);

    Pt2.Add(Points[1]);
    Pt2.Add(Point2D(Points[1].X-cos(ang+angB)*arr,Points[1].Y-sin(ang+angB)*arr));
    Pt2.Add(Point2D(Points[1].X-cos(-ang+angB)*arr,Points[1].Y+sin(-ang+angB)*arr));
    Pt2.DrawAsPolygon(Cnv,ClipREct2D,Box,VT);

    testang := RadToDEg(ang);

    if ang >=0 then
     TmpRect2D:= Rect2D( (Points[0].X+Points[1].X)/2-cos(pi/2-ang)*5,(Points[0].Y+Points[1].Y)/2,(Points[0].X+Points[1].X)/2+1,(Points[0].Y+Points[1].Y)/2+sin(pi/2-ang)*txt.Height);
    if ang < 0 then
     TmpRect2D:= Rect2D((Points[0].X+Points[1].X)/2+cos(pi/2+ang)*txt.Height,(Points[0].Y+Points[1].Y)/2,(Points[0].X+Points[1].X)/2,(Points[0].Y+Points[1].Y)/2+sin(pi/2+ang)*txt.Height);

    Pt2.Clear;
    Pt2.Add(Point2D(TmpREct2d.Left,TmpRect2D.Top));
    Pt2.Add(Point2D(TmpREct2d.Left,TmpRect2D.Bottom));
    Pt2.Add(Point2D(TmpREct2d.Right,TmpRect2D.Bottom));
    Pt2.Add(Point2D(TmpREct2d.Right,TmpRect2D.Top));
    Pt2.DrawAsPolyline(Cnv,ClipRect2D,Box,VT);

    txt.WritableBox := TmpRect2D;
    if ang <0 then ang := 2*pi+ang;
    //txt.LogFont.Escapement := Round(RadToDeg(ang)*10);
    txt.Angle := Round(RadToDeg(ang));;
    txt.Text := FloatTostr(RoundTo(sqrt(power(DeltaY,2)+power(DeltaX,2)),-2));

    if not HasTransform then
     //DrawText2D(Cnv, Points[0], Points[1], ClipRect2D, VT, txt)
    else
     //DrawText2D(Cnv, Points[0], Points[1], ClipRect2D, MultiplyTransform2D(ModelTransform, VT), txt);
  end;

//TCoordPoints2DInsp////////////////////////////////////////////////////////////
constructor TCoordPoints2DInsp.create(AOwner: TGraphicObject);
var fOwner: TGraphicObject; TmpPointCount: word;
begin
  inherited Create;
  fOwner := AOwner;
  TmpPointCount := TPrimitive2D(fOwner).Points.Count;
  fStartPoint  := TCoordPoint2DInsp.create(fOwner, POINT_CODE_START_POINT);
  fEndPoint    := TCoordPoint2DInsp.create(fOwner, POINT_CODE_END_POINT);
  //fMiddlePoint    := TCoordPoint2DInsp.create(fOwner, POINT_CODE_MIDDLE_POINT)
end;

destructor TCoordPoints2DInsp.destroy;
begin
  fStartPoint.Free;
  fEndPoint.Free;
  inherited;
end;

//TCoordPoint2DInsp/////////////////////////////////////////////////////////////
//Chage PointValue
constructor TCoordPoint2DInsp.create(AOwner: TGraphicObject; APointCode: integer);
begin
  inherited create;
  fOwner := AOwner;
  fPointCode := APointCode;
end;

function  TCoordPoint2DInsp.GetX: TRealType;
begin
  case fPointCode of
    POINT_CODE_START_POINT:   result := TPrimitive2D(fOwner).StartPointX;
    POINT_CODE_END_POINT:     result := TPrimitive2D(fOwner).EndPointX;
    POINT_CODE_MIDDLE_POINT:  result := TPrimitive2D(fOwner).MiddlePoint.X;
    POINT_CODE_BASE_POINT: begin
      if (fOwner is TBlock2D) then result := TBlock2D(fOwner).OriginPoint.X
      else if (fOwner is TJustifiedVectText2D) then result := TJustifiedVectText2D(fOwner).BasePoint.X
    end
    else result := TPrimitive2D(fOwner).Points[fPointCode].X;
  end;
end;

function  TCoordPoint2DInsp.GetY: TRealType;
begin
  case fPointCode of
    POINT_CODE_START_POINT:   result := TPrimitive2D(fOwner).StartPointY;
    POINT_CODE_END_POINT:     result := TPrimitive2D(fOwner).EndPointY;
    POINT_CODE_MIDDLE_POINT:  result := TPrimitive2D(fOwner).MiddlePoint.Y;
    POINT_CODE_BASE_POINT: begin
      if (fOwner is TBlock2D) then result := TBlock2D(fOwner).OriginPoint.X
      else if (fOwner is TJustifiedVectText2D) then result := TJustifiedVectText2D(fOwner).BasePoint.Y
    end
    else result := TPrimitive2D(fOwner).Points[fPointCode].Y;
  end;
end;

procedure TCoordPoint2DInsp.SetX(AValue: TRealType);
begin
  TPrimitive2D(fOwner).Points.DisableEvents := true;
  case fPointCode of
    POINT_CODE_START_POINT:  TPrimitive2D(fOwner).StartPointX   := AValue;
    POINT_CODE_END_POINT:    TPrimitive2D(fOwner).EndPointX     := AValue;
    POINT_CODE_MIDDLE_POINT: TPrimitive2D(fOwner).MiddlePointX  := AValue;
    POINT_CODE_BASE_POINT: begin
      if (fOwner is TBlock2D) then TBlock2D(fOwner).OriginPoint := Point2D(AValue, TBlock2D(fOwner).OriginPoint.Y)
      else if (fOwner is TJustifiedVectText2D) then TJustifiedVectText2D(fOwner).BasePoint := Point2D(AValue, TJustifiedVectText2D(fOwner).BasePoint.Y)
    end
    else PVectPoints2D(TPrimitive2D(fOwner).Points.PointsReference)^[fPointCode].X := AValue;
  end;
  TPrimitive2D(fOwner).Points.DisableEvents := false;
  TPrimitive2D(fOwner).UpdateExtension(nil);
end;

procedure TCoordPoint2DInsp.SetY(AValue: TRealType);
begin
  TPrimitive2D(fOwner).Points.DisableEvents := true;
  case fPointCode of
    POINT_CODE_START_POINT:  TPrimitive2D(fOwner).StartPointY   := AValue;
    POINT_CODE_END_POINT:    TPrimitive2D(fOwner).EndPointY     := AValue;
    POINT_CODE_MIDDLE_POINT: TPrimitive2D(fOwner).MiddlePointY  := AValue;
    POINT_CODE_BASE_POINT: begin
      if (fOwner is TBlock2D) then TBlock2D(fOwner).OriginPoint := Point2D(TBlock2D(fOwner).OriginPoint.X, AValue)
      else if (fOwner is TJustifiedVectText2D) then TJustifiedVectText2D(fOwner).BasePoint := Point2D(TJustifiedVectText2D(fOwner).BasePoint.X, AValue)
    end
    else PVectPoints2D(TPrimitive2D(fOwner).Points.PointsReference)^[fPointCode].Y := AValue;
  end;
  TPrimitive2D(fOwner).Points.DisableEvents := false;
  TPrimitive2D(fOwner).UpdateExtension(nil);
end;

//The OwnerObject will be moved
//TCoordPoint2DMoveObjectInsp = class
constructor TCoordPoint2DMoveObjectInsp.create(AOwner: TGraphicObject; APointIndex: word);
begin
  fOwner        := AOwner;
  fPointIndex   := APointIndex;
end;

function  TCoordPoint2DMoveObjectInsp.GetX: TRealType;
var TmpPoint2D: TPoint2D;
begin
  TmpPoint2D := TransformPoint2D(TPrimitive2D(fOwner).Points[fPointIndex], TPrimitive2D(fOwner).ModelTransform);
  result := TmpPoint2D.X;
end;

function  TCoordPoint2DMoveObjectInsp.GetY: TrealType;
var TmpPoint2D: TPoint2D;
begin
  TmpPoint2D := TransformPoint2D(TPrimitive2D(fOwner).Points[fPointIndex], TPrimitive2D(fOwner).ModelTransform);
  result := TmpPoint2D.Y;
end;

procedure TCoordPoint2DMoveObjectInsp.SetX(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := TPrimitive2D(fOwner).Points[fPointIndex];
  ToPt := Point2D(AValue, GetY);
  ToPt := TObject2D(fOwner).WorldToObject(ToPt);
  TPrimitive2D(fOwner).MoveTo(ToPt, DragPt);
end;

procedure TCoordPoint2DMoveObjectInsp.SetY(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := TPrimitive2D(fOwner).Points[fPointIndex];
  ToPt := Point2D(GetX, AValue);
  ToPt := TObject2D(fOwner).WorldToObject(ToPt);
  TPrimitive2D(fOwner).MoveTo(ToPt, DragPt);
end;

//TPenInsp
constructor TPenInsp.create(AOwner: TGraphicObject);
begin
  inherited create;
  fOwner := AOwner;
end;

destructor  TPenInsp.destroy;
begin
  inherited;
end;

function   TPenInsp.GetPenSource: TPenSource;
begin
  result :=  TPrimitive2D(fOwner).PenSource;
end;

procedure  TPenInsp.SetPenSource(APenSource: TPenSource);
begin
  TPrimitive2D(fOwner).PenSource := APenSource;
end;

function  TPenInsp.GetPenColor:  TColor;
begin
  result :=  TPrimitive2D(fOwner).PenColor;
end;

procedure TPenInsp.SetPenColor(APenColor: TColor);
begin
  TPrimitive2D(fOwner).PenColor := APenColor;
end;

function  TPenInsp.GetPenStyle:  TPenStyle;
begin
  result :=  TPrimitive2D(fOwner).PenStyle;
end;

procedure TPenInsp.SetPenStyle(APenStyle: TPenStyle);
begin
  TPrimitive2D(fOwner).PenStyle := APenStyle;
end;

function  TPenInsp.GetPenWidth: word;
begin
  result :=  TPrimitive2D(fOwner).PenWidth;
end;

procedure TPenInsp.SetPenWidth(APenWidht: word);
begin
  TPrimitive2D(fOwner).PenWidth := APenWidht;
end;


//TBrushInspClosedCurve2D
constructor TBrushInspClosedCurve2D.create(AOwner: TGraphicObject);
begin
  inherited create;
  fOwner := AOwner;
end;

destructor  TBrushInspClosedCurve2D.destroy;
begin
  inherited;
end;

function   TBrushInspClosedCurve2D.GetBrushSource: TBrushSource;
begin
  result :=  TClosedCurve2D(fOwner).BrushSource;
end;

procedure  TBrushInspClosedCurve2D.SetBrushSource(ABrushSource: TBrushSource);
begin
  TClosedCurve2D(fOwner).BrushSource := ABrushSource;
end;

function  TBrushInspClosedCurve2D.GetBrushColor:  TColor;
begin
  result :=  TClosedCurve2D(fOwner).BrushColor;
end;

procedure TBrushInspClosedCurve2D.SetBrushColor(ABrushColor: TColor);
begin
  TClosedCurve2D(fOwner).BrushColor := ABrushColor;
end;

function  TBrushInspClosedCurve2D.GetBrushStyle:  TBrushStyle;
begin
  result :=  TClosedCurve2D(fOwner).BrushStyle;
end;

procedure TBrushInspClosedCurve2D.SetBrushStyle(ABrushStyle: TBrushStyle);
begin
  TClosedCurve2D(fOwner).BrushStyle := ABrushStyle;
end;


//TBrushInspClosedPolyline2D
constructor TBrushInspClosedPolyline2D.create(AOwner: TGraphicObject);
begin
  inherited create;
  fOwner := AOwner;
end;

destructor  TBrushInspClosedPolyline2D.destroy;
begin
  inherited;
end;

function   TBrushInspClosedPolyline2D.GetBrushSource: TBrushSource;
begin
  result :=  TClosedPolyline2D(fOwner).BrushSource;
end;

procedure  TBrushInspClosedPolyline2D.SetBrushSource(ABrushSource: TBrushSource);
begin
  TClosedPolyline2D(fOwner).BrushSource := ABrushSource;
end;

function  TBrushInspClosedPolyline2D.GetBrushColor:  TColor;
begin
  result :=  TClosedPolyline2D(fOwner).BrushColor;
end;

procedure TBrushInspClosedPolyline2D.SetBrushColor(ABrushColor: TColor);
begin
  TClosedPolyline2D(fOwner).BrushColor := ABrushColor;
end;

function  TBrushInspClosedPolyline2D.GetBrushStyle:  TBrushStyle;
begin
  result :=  TClosedPolyline2D(fOwner).BrushStyle;
end;

procedure TBrushInspClosedPolyline2D.SetBrushStyle(ABrushStyle: TBrushStyle);
begin
  TClosedPolyline2D(fOwner).BrushStyle := ABrushStyle;
end;


//TPrim2DInsp///////////////////////////////////////////////////////////////////
constructor TPrim2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fPenInsp := TPenInsp.Create(fOwner);
end;

destructor  TPrim2DInsp.destroy;
begin
  fPenInsp.free;
  inherited;
end;


//TSimplePrim2DInsp/////////////////////////////////////////////////////////////
constructor TSimplePrim2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fCoordPoints2DInsp := TCoordPoints2DInsp.create(fOwner);
end;

destructor  TSimplePrim2DInsp.destroy;
begin
  fCoordPoints2DInsp.Free;
  inherited;
end;

function   TSimplePrim2DInsp.GetShowDirection: boolean;
begin
  result := TSimplePrimitive2D(fOwner).ShowDirection;
end;

procedure  TSimplePrim2DInsp.SetShowDirection(AValue: boolean);
begin
  TSimplePrimitive2D(fOwner).ShowDirection := AValue;
end;

function   TSimplePrim2DInsp.GetObjectLength: TRealType;
begin
  result := TSimplePrimitive2D(fOwner).ObjectLength;
end;

//TLine2DInsp.
constructor TLine2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
end;
destructor  TLine2DInsp.destroy;
begin
  inherited;
end;

//TClosedPolyline2DInsp
constructor TClosedPolyline2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fBrushInsp := TBrushInspClosedPolyline2D.create(fOwner);
end;

destructor  TClosedPolyline2DInsp.destroy;
begin
  fBrushInsp.Free;
  inherited;
end;

function  TClosedPolyline2DInsp.GetArea: TRealType;
begin
  result := TClosedPolyline2D(fOwner).GetArea;
end;

function  TClosedPolyline2DInsp.GetDirection: TArcDirection;
begin
 result := TClosedPolyline2D(fOwner).Direction;
end;

procedure TClosedPolyline2DInsp.SetDirection(AArcDirection: TArcDirection);
begin
 TClosedPolyline2D(fOwner).Direction := AArcDirection;
end;

//TDirectionalCurve2DInsp
constructor TDirectionalCurve2DInsp.create(AOwner: TGraphicObject);
begin
   fOwner := AOwner;
end;

destructor  TDirectionalCurve2DInsp.destroy;
begin
  inherited;
end;

function   TDirectionalCurve2DInsp.GetDirection: TArcDirection;
begin
  result := TDirectionalCurve2D(fOwner).Direction;
end;

procedure  TDirectionalCurve2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TDirectionalCurve2D(fOwner).Direction := ADirection;
end;

//TEllipse2DInsp.
constructor TEllipse2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
end;

destructor  TEllipse2DInsp.destroy;
begin
  inherited;
end;

function   TEllipse2DInsp.GetWidth: TRealType;
begin
  result := TEllipse2D(fOwner).Width;
end;

procedure  TEllipse2DInsp.SetWidth(AValue: TRealType);
begin
  TEllipse2D(fOwner).Width := AValue;
end;

function   TEllipse2DInsp.GetHeight: TRealType;
begin
  result := TEllipse2D(fOwner).Height;
end;

procedure  TEllipse2DInsp.SetHeight(AValue: TRealType);
begin
  TEllipse2D(fOwner).Height := AValue;
end;

function  TEllipse2DInsp.GetDirection: TArcDirection;
begin
  result := TEllipse2D(fOwner).Direction;
end;

procedure  TEllipse2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TEllipse2D(fOwner).Direction := ADirection;
end;

function   TEllipse2DInsp.GetCurvePrecision: word;
begin
  result := TEllipse2D(fOwner).CurvePrecision;
end;

procedure  TEllipse2DInsp.SetCurvePrecision(AValue: word);
begin
  TEllipse2D(fOwner).CurvePrecision := AValue;
end;

//TFrame2DInsp.
constructor TFrame2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
end;

destructor  TFrame2DInsp.destroy;
begin
  inherited;
end;

function   TFrame2DInsp.GetWidth: TRealType;
begin
  result := TFrame2D(fOwner).Width;
end;

procedure  TFrame2DInsp.SetWidth(AValue: TRealType);
begin
  TFrame2D(fOwner).Width := AValue;
end;

function   TFrame2DInsp.GetHeight: TRealType;
begin
  result := TFrame2D(fOwner).Height;
end;

procedure  TFrame2DInsp.SetHeight(AValue: TRealType);
begin
  TFrame2D(fOwner).Height := AValue;
end;

function  TFrame2DInsp.GetDirection: TArcDirection;
begin
  result := TFrame2D(fOwner).Direction;
end;

procedure  TFrame2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TFrame2D(fOwner).Direction := ADirection;
end;

function   TFrame2DInsp.GetChamfered: boolean;
begin
  result := TFrame2D(fOwner).Chamfered;
end;

procedure  TFrame2DInsp.SetfChamfered(AValue: boolean);
begin
  TFrame2D(fOwner).Chamfered := AValue;
end;

function   TFrame2DInsp.GetChamferValue: TRealType;
begin
  result := TFrame2D(fOwner).ChamferValue;
end;

procedure  TFrame2DInsp.SetChamferValue(AValue: TRealType);
begin
  TFrame2D(fOwner).ChamferValue := AValue;
end;

function   TFrame2DInsp.GeStartCorner: TFrameStartCorner;
begin
  result := TFrame2D(fOwner).StartCorner;
end;

procedure  TFrame2DInsp.SetStartCorner(AValue: TFrameStartCorner);
begin
  TFrame2D(fOwner).StartCorner := AValue;
end;

//TArcAngles////////////////////////////////////////////////////////////////////
constructor TArcAngles.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
end;

function  TArcAngles.GetStartAngle: TRealType;
begin
  if (fOwner is TCircularArc2D) then
    result := TCircularArc2D(fOwner).StartAngle
  else if (fOwner is TEllipticalArc2D) then
    result := TEllipticalArc2D(fOwner).StartAngle
  else if (fOwner is TSegment2D) then
    result := TSegment2D(fOwner).StartAngle
  else if (fOwner is TSector2D) then
    result := TSector2D(fOwner).StartAngle
end;

procedure TArcAngles.SetStartAngle(AAngle: TRealType);
begin
  if (fOwner is TCircularArc2D) then
    TCircularArc2D(fOwner).StartAngle :=  AAngle
  else if (fOwner is TEllipticalArc2D) then
    TEllipticalArc2D(fOwner).StartAngle :=  AAngle
  else if (fOwner is TSegment2D) then
    TSegment2D(fOwner).StartAngle :=  AAngle
  else if (fOwner is TSector2D) then
    TSector2D(fOwner).StartAngle :=  AAngle
end;

function  TArcAngles.GetEndAngle: TRealType;
begin
  if (fOwner is TCircularArc2D) then
    result := TCircularArc2D(fOwner).EndAngle
  else if (fOwner is TEllipticalArc2D) then
    result := TEllipticalArc2D(fOwner).EndAngle
  else if (fOwner is TSegment2D) then
    result := TSegment2D(fOwner).EndAngle
  else if (fOwner is TSector2D) then
    result := TSector2D(fOwner).EndAngle
end;

procedure TArcAngles.SetEndAngle(AAngle: TRealType);
begin
  if (fOwner is TCircularArc2D) then
    TCircularArc2D(fOwner).EndAngle :=  AAngle
  else if (fOwner is TEllipticalArc2D) then
    TEllipticalArc2D(fOwner).EndAngle :=  AAngle
  else if (fOwner is TSegment2D) then
    TSegment2D(fOwner).EndAngle :=  AAngle
  else if (fOwner is TSector2D) then
    TSector2D(fOwner).EndAngle :=  AAngle
end;

function  TArcAngles.GetArcAngle: TRealType;
begin
  if (fOwner is TCircularArc2D) then
    result := TCircularArc2D(fOwner).ArcAngle
  else if (fOwner is TEllipticalArc2D) then
    result := TEllipticalArc2D(fOwner).ArcAngle
  else if (fOwner is TSegment2D) then
    result := TSegment2D(fOwner).ArcAngle
  else if (fOwner is TSector2D) then
    result := TSector2D(fOwner).ArcAngle
end;

procedure TArcAngles.SetArcAngle(AAngle: TRealType);
begin
  if (fOwner is TCircularArc2D) then
    TCircularArc2D(fOwner).ArcAngle :=  AAngle
  else if (fOwner is TEllipticalArc2D) then
    TEllipticalArc2D(fOwner).ArcAngle :=  AAngle
  else if (fOwner is TSegment2D) then
    TSegment2D(fOwner).ArcAngle :=  AAngle
  else if (fOwner is TSector2D) then
    TSector2D(fOwner).ArcAngle :=  AAngle
end;

//TEllipticalArc2DInsp
constructor TEllipticalArc2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fArcAngles      :=  TArcAngles.create(fOwner);
end;

destructor TEllipticalArc2DInsp.destroy;
begin
  fArcAngles.Free;
  inherited;
end;

function  TEllipticalArc2DInsp.GetDirection: TArcDirection;
begin
  result := TEllipticalArc2D(fOwner).Direction;
end;

procedure TEllipticalArc2DInsp.SetDirection(AArcDirection: TArcDirection);
begin
  TEllipticalArc2D(fOwner).Direction := AArcDirection;
end;

function   TEllipticalArc2DInsp.GetCurvePrecision: word;
begin
  result := TEllipticalArc2D(fOwner).CurvePrecision;
end;

procedure  TEllipticalArc2DInsp.SetCurvePrecision(AValue: word);
begin
  TEllipticalArc2D(fOwner).CurvePrecision := AValue;
end;

//TClosedCurve2DInsp
constructor TClosedCurve2DInsp.create(AOwner: TGraphicObject);
begin
  inherited create;
  fOwner := AOwner;
  fBrushInsp := TBrushInspClosedCurve2D.create(fOwner);
end;

destructor  TClosedCurve2DInsp.destroy;
begin
  fBrushInsp.Free;
  inherited;
end;

function  TClosedCurve2DInsp.GetArea:  TRealType;
begin
  result := TClosedCurve2D(fOwner).Area;
end;


//TCircle2DInsp
constructor TCircle2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fCenterPoint  := TCoordPoint2DMoveObjectInsp.create(fOwner, 0);
end;

destructor  TCircle2DInsp.destroy;
begin
  fCenterPoint.Free;
  inherited;
end;

function  TCircle2DInsp.GetDirection: TArcDirection;
begin
  result := TCircle2D(fOwner).Direction;
end;

procedure TCircle2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TCircle2D(fOwner).Direction := ADirection;
end;

function  TCircle2DInsp.GetRadius: TrealType;
begin
  result := TCircle2D(fOwner).Radius;
end;

procedure TCircle2DInsp.SetRadius(AValue: TRealType);
begin
  TCircle2D(fOwner).Radius := AValue;
end;

function  TCircle2DInsp.GetStartAngle: TrealType;
begin
  result := TCircle2D(fOwner).StartAngle;
end;

procedure TCircle2DInsp.SetStartAngle(AValue: TRealType);
begin
  TCircle2D(fOwner).StartAngle := AValue;
end;

function   TCircle2DInsp.GetCurvePrecision: word;
begin
  result := TCircle2D(fOwner).CurvePrecision;
end;

procedure  TCircle2DInsp.SetCurvePrecision(AValue: word);
begin
  TCircle2D(fOwner).CurvePrecision := AValue;
end;

//TCircularArc2DInsp
constructor TCircularArc2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fCenterPoint  := TCoordPoint2DMoveObjectInsp.create(fOwner, 0);
  fArcAngles := TArcAngles.create(fOwner);
end;

destructor  TCircularArc2DInsp.destroy;
begin
  fCenterPoint.Free;
  fArcAngles.Free;
  inherited;
end;

function  TCircularArc2DInsp.GetDirection: TArcDirection;
begin
  result := TCircularArc2D(fOwner).Direction;
end;

procedure TCircularArc2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TCircularArc2D(fOwner).Direction := ADirection;
end;

function  TCircularArc2DInsp.GetRadius: TrealType;
begin
  result := TCircularArc2D(fOwner).Radius;
end;

procedure TCircularArc2DInsp.SetRadius(AValue: TRealType);
begin
  TCircularArc2D(fOwner).Radius := AValue;
end;

function   TCircularArc2DInsp.GetCurvePrecision: word;
begin
  result := TCircularArc2D(fOwner).CurvePrecision;
end;

procedure  TCircularArc2DInsp.SetCurvePrecision(AValue: word);
begin
  TCircularArc2D(fOwner).CurvePrecision := AValue;
end;


//TSector2DInsp
constructor TSector2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fCenterPoint  := TCoordPoint2DMoveObjectInsp.create(fOwner, 0);
  fArcAngles    := TArcAngles.create(fOwner);
end;

destructor  TSector2DInsp.destroy;
begin
  fCenterPoint.Free;
  fArcAngles.Free;
  inherited;
end;

function  TSector2DInsp.GetDirection: TArcDirection;
begin
  result := TSector2D(fOwner).Direction;
end;

procedure TSector2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TSector2D(fOwner).Direction := ADirection;
end;

function  TSector2DInsp.GetRadius: TrealType;
begin
  result := TSector2D(fOwner).Radius;
end;

procedure TSector2DInsp.SetRadius(AValue: TRealType);
begin
  TSector2D(fOwner).Radius := AValue;
end;

function  TSector2DInsp.GetStartAngle: TrealType;
begin
  result := TSector2D(fOwner).StartAngle;
end;

procedure TSector2DInsp.SetStartAngle(AValue: TRealType);
begin
  TSector2D(fOwner).StartAngle := AValue;
end;

function  TSector2DInsp.GetEndAngle: TrealType;
begin
  result := TSector2D(fOwner).EndAngle;
end;

procedure TSector2DInsp.SetEndAngle(AValue: TRealType);
begin
  TSector2D(fOwner).EndAngle := AValue;
end;

function   TSector2DInsp.GetCurvePrecision: word;
begin
  result := TSector2D(fOwner).CurvePrecision;
end;

procedure  TSector2DInsp.SetCurvePrecision(AValue: word);
begin
  TSector2D(fOwner).CurvePrecision := AValue;
end;


//TSegmet2DInsp
constructor TSegment2DInsp.create(AOwner: TGraphicObject);
begin
  fOwner := AOwner;
  fCenterPoint  := TCoordPoint2DMoveObjectInsp.create(fOwner, 0);
  fArcAngles    := TArcAngles.create(fOwner);
end;

destructor  TSegment2DInsp.destroy;
begin
  fCenterPoint.Free;
  fArcAngles.Free;
  inherited;
end;

function  TSegment2DInsp.GetDirection: TArcDirection;
begin
  result := TSegment2D(fOwner).Direction;
end;

procedure TSegment2DInsp.SetDirection(ADirection: TArcDirection);
begin
  TSegment2D(fOwner).Direction := ADirection;
end;

function  TSegment2DInsp.GetRadius: TrealType;
begin
  result := TSegment2D(fOwner).Radius;
end;

procedure TSegment2DInsp.SetRadius(AValue: TRealType);
begin
  TSegment2D(fOwner).Radius := AValue;
end;

function  TSegment2DInsp.GetStartAngle: TrealType;
begin
  result := TSegment2D(fOwner).StartAngle;
end;

procedure TSegment2DInsp.SetStartAngle(AValue: TRealType);
begin
  TSegment2D(fOwner).StartAngle := AValue;
end;

function  TSegment2DInsp.GetEndAngle: TrealType;
begin
  result := TSegment2D(fOwner).EndAngle;
end;

procedure TSegment2DInsp.SetEndAngle(AValue: TRealType);
begin
  TSegment2D(fOwner).EndAngle := AValue;
end;

function   TSegment2DInsp.GetCurvePrecision: word;
begin
  result := TSegment2D(fOwner).CurvePrecision;
end;

procedure  TSegment2DInsp.SetCurvePrecision(AValue: word);
begin
  TSegment2D(fOwner).CurvePrecision := AValue;
end;


//TSimplePrimitive2D////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
procedure   TSimplePrimitive2D.Reverse;
begin
  self.Points.Reverse;
end;

procedure TSimplePrimitive2D.Inverse;
begin
  self.Points.Reverse;
end;

procedure TSimplePrimitive2D.Explode(ADeleteSource: boolean);
begin
  //
end;

procedure TSimplePrimitive2D.InitializeAngle;
begin
  fAngle := 0;
end;

constructor TSimplePrimitive2D.create(ID: LongInt; NPts: Integer);
begin
  inherited create(ID, NPts);
  fSimplePrim2DInsp := TSimplePrim2DInsp.create(self);
end;

destructor  TSimplePrimitive2D.destroy;
begin
  fSimplePrim2DInsp.Free;
  inherited;
end;

procedure TSimplePrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TSimplePrimitive2D) then
  begin // Per default non aggiunge i punti.
    if not Assigned(fPoints) then
    begin
      fPoints := CreateVect(0);
      fPoints.GrowingEnabled := True;
      fPoints.OnChange := UpdateExtension;
    end;
    LayerName      := (Obj as TSimplePrimitive2D).LayerName;
    fShowDirection := (Obj as TSimplePrimitive2D).ShowDirection;
    fPoints.Clear;
  end;
end;

constructor TSimplePrimitive2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var TmpWord: Word; Cont: Integer; TmpPt: TPoint2D; TmpBoolean: Boolean;
    TmpReal: TRealType;
begin
  { Load the standard properties }
  inherited;
  fSimplePrim2DInsp := TSimplePrim2DInsp.create(self);
  with Stream do
   begin
     Read(TmpBoolean, SizeOf(TmpBoolean));
     fShowDirection := TmpBoolean;
   end;
end;

procedure TSimplePrimitive2D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
  TmpReal: TrealType;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpBoolean :=  fShowDirection;
     Write(TmpBoolean, SizeOf(TmpBoolean));
   end;
end;

function  TSimplePrimitive2D.GetLength: TRealType;
var i: integer;  hPoints: TPointsSet2D;
begin
  result := 0;
  if (self is TLine2D) or (self is TPolyline2D) or (self is TPolygon2D) then
     hPoints := TLine2D(self).Points
  else if (self is TFrame2D) or (self is TRectangle2D) then
     hPoints := TFrame2D(self).ProfilePoints
  else if (self is TOutline2D) then
       hPoints := TOutline2D(self).ProfilePoints
  else hPoints := self.Points;
  for i := 1 to hPoints.Count - 1 do
    result := result + PointDistance2D(hPoints[i-1], hPoints[i]);
end;

function TSimplePrimitive2D.GetStartPoint: TPoint2D;
begin
  if self is TLine2D then
    result := TransformPoint2D(Points[0], ModelTransform)
  else  if (self is TOutline2D) then
    result := TransformPoint2D(TOutline2D(self).ProfilePoints[0], ModelTransform)
end;

function TSimplePrimitive2D.GetStartPointX: TRealType;
begin
  result := GetStartPoint.X;
end;

function TSimplePrimitive2D.GetStartPointY: TRealType;
begin
  result := GetStartPoint.Y
end;

procedure TSimplePrimitive2D.SetStartPointX(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetStartPoint.Y - AValue;
  self.Points.DisableEvents := true;
  PVectPoints2D(self.Points.PointsReference)^[0].X := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;

procedure TSimplePrimitive2D.SetStartPointY(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetStartPoint.Y - AValue;
  self.Points.DisableEvents := true;
  PVectPoints2D(self.Points.PointsReference)^[0].Y := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;

function TSimplePrimitive2D.GetEndPoint: TPoint2D;
begin
  if (self is TLine2D) then
    result := TransformPoint2D(Points[1], ModelTransform)
  else  if (self is TOutline2D) then
    result := TransformPoint2D(TOutline2D(self).ProfilePoints[TOutline2D(self).ProfilePoints.Count - 1], ModelTransform)
end;

function TSimplePrimitive2D.GetEndPointX: TRealType;
begin
  result := GetEndPoint.X
end;

function TSimplePrimitive2D.GetEndPointY: TRealType;
begin
  result := GetEndPoint.Y
end;

procedure TSimplePrimitive2D.SetEndPointX(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetEndPoint.X - AValue;
  self.Points.DisableEvents := true;
  count := self.Points.Count;
  PVectPoints2D(self.Points.PointsReference)^[count-1].X := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;

procedure TSimplePrimitive2D.SetEndPointY(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetEndPoint.Y - AValue;
  self.Points.DisableEvents := true;
  count := self.Points.Count;
  PVectPoints2D(self.Points.PointsReference)^[count-1].Y := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;
//end   TSimplePrimitive2D


////////////////////////////////////////////////////////////////////////////////
// TDirectionalCurve2D
procedure TDirectionalCurve2D.Reverse; //added
begin
  if fDirection = adClockwise then
    fDirection := adCounterClockwise
  else
    fDirection := adClockwise;
  UpdateExtension(self);
end;

procedure TDirectionalCurve2D.Inverse; //added
begin
  if fDirection = adClockwise then
    fDirection := adCounterClockwise
  else
    fDirection := adClockwise;
  UpdateExtension(self);
end;

constructor TDirectionalCurve2D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
begin
  inherited Create(ID, NPts, CurvePrec);
  fDirectionalCurve2DInsp := TDirectionalCurve2DInsp.create(self);
  fDirection := adClockwise;
end;

destructor TDirectionalCurve2D.destroy;
begin
  fDirectionalCurve2DInsp.Free;
  inherited destroy;
end;

constructor TDirectionalCurve2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited CreateFromStream(Stream, Version);
  fDirectionalCurve2DInsp := TDirectionalCurve2DInsp.create(self);
end;

procedure TDirectionalCurve2D.SetDirection(ArcDirection: TArcDirection);
begin
  fDirection := ArcDirection;
  UpdateExtension(Self);
end;

procedure TDirectionalCurve2D.SetStartPointX(AValue:TRealType);
begin

end;

procedure TDirectionalCurve2D.SetStartPointY(AValue:TRealType);
begin

end;

procedure TDirectionalCurve2D.SetEndPointX(AValue:TRealType);
begin

end;

procedure TDirectionalCurve2D.SetEndPointY(AValue:TRealType);
begin

end;


//TClosedCurve2D
procedure   TClosedCurve2D.Explode(ADeleteSource: boolean);
var TmpPolygon2D: TPolygon2D;  i: integer;
begin
  BeginUseProfilePoints;
  TmpPolygon2D := TPolygon2D.Create(-1, []);
  TmpPolygon2D.Assign(self);
  TmpPolygon2D.Transform(ModelTransform);
  EndUseProfilePoints;
  TCADCmp2D(OwnerCAD).AddObject(-1, TmpPolygon2D);
end;

constructor TClosedCurve2D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
begin
  inherited Create(ID, NPts, CurvePrec);
  fClosedCurve2DInsp := TClosedCurve2DInsp.create(self);
end;

constructor TClosedCurve2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpBrushSource: TBrushSource;
  TmpBrushColor:  TColor;
  TmpBrushStyle:  TBrushStyle;
begin
  inherited  CreateFromStream(Stream, Version);
  fClosedCurve2DInsp := TClosedCurve2DInsp.create(self);

  Stream.Read(TmpBrushSource, SizeOf(TmpBrushSource));
  self.fBrushSource := TmpBrushSource;
  Stream.Read(TmpBrushColor, SizeOf(TmpBrushColor));
  self.BrushColor := TmpBrushColor;
  Stream.Read(TmpBrushStyle, SizeOf(TmpBrushStyle));
  self.BrushStyle := TmpBrushStyle;
end;

procedure TClosedCurve2D.SaveToStream(const Stream: TStream);
var
  TmpBrushSource: TBrushSource;
  TmpBrushColor:  TColor;
  TmpBrushStyle:  TBrushStyle;
begin
  inherited  SaveToStream(Stream);
  TmpBrushSource := self.fBrushSource;
  Stream.Write(TmpBrushSource, SizeOf(TmpBrushSource));
  TmpBrushColor := self.fBrushColor;
  Stream.Write(TmpBrushColor, SizeOf(TmpBrushColor));
  TmpBrushStyle := self.fBrushStyle;
  Stream.Write(TmpBrushStyle, SizeOf(TmpBrushStyle));
end;

procedure TClosedCurve2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TClosedCurve2D) then
  begin
    fBrushSource :=  (Obj as TClosedCurve2D).BrushSource;
    fBrushColor  :=  (Obj as TClosedCurve2D).BrushColor;
    fBrushStyle  :=  (Obj as TClosedCurve2D).BrushStyle;
  end;
end;

destructor  TClosedCurve2D.destroy;
begin
  fClosedCurve2DInsp.Free;
  inherited destroy;
end;

function TClosedCurve2D.GetArea: TRealType;
begin
  result := ObjectArea(ProfilePoints);
end;

procedure TClosedCurve2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var P0, P1: TPoint2D;  x, y, TmpArrowLength: TRealType;
  TmpPenColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenWidth: word;
  TmpPenMode: TPenMode;
  TmpCosmetic: boolean;
  TmpPenEndCap: TFPPenEndCap;
  TmpJoinStyle : TFPPenJoinStyle;

  TmpBrushColor: TColor;
  TmpBrushStyle: TBrushStyle;
begin
  case self.fBrushSource of
    bsByLayer: begin
      //defaul Value is ByLayer
    end;
    bsByBlock: begin
      fBrushSource := bsByLayer;
      exit; //not implemented.
    end;
    bsCustom: begin
      TmpBrushColor := Cnv.Canvas.Brush.Color;
      TmpBrushStyle := Cnv.Canvas.Brush.Style;
      Cnv.Canvas.Brush.Color := self.BrushColor;
      Cnv.Canvas.Brush.Style := self.BrushStyle;
    end;
    else begin
      //
    end;
  end;

  case fPenSource of
    psByLayer: begin
      //defaul Value is ByLayer
    end;
    psByBlock: begin
      fPenSource := psByLayer;
      exit; //not implemented.
    end;
    psCustom: begin
        //TmpPen := TPen.Create;
         TmpPenColor  := Cnv.Canvas.Pen.Color;
         TmpPenStyle  := Cnv.Canvas.Pen.Style;
         TmpPenWidth  := Cnv.Canvas.Pen.Width;
         {TmpPenMode   := Cnv.Canvas.Pen.Mode;
         TmpJoinStyle := Cnv.Canvas.Pen.JoinStyle;
         TmpPenEndCap := Cnv.Canvas.Pen.EndCap;
         TmpCosmetic  := Cnv.Canvas.Pen.Cosmetic;}
         case  DrawMode of
           DRAWMODE_EDIT: begin
             //Cnv.Canvas.Pen.Color := EditPenColor;
             //Cnv.Canvas.Pen.Style := EditPenStyle;
             //Cnv.Canvas.Pen.Width := EditPenWidth;
           end;
           DRAWMODE_NORMAL: begin
             Cnv.Canvas.Pen.Color     := self.PenColor;
             Cnv.Canvas.Pen.Style     := self.PenStyle;
             Cnv.Canvas.Pen.Width     := self.PenWidth;
             {Cnv.Canvas.Pen.Mode      := self.Pen.Mode;
             Cnv.Canvas.Pen.JoinStyle := self.Pen.JoinStyle;
             Cnv.Canvas.Pen.EndCap    := self.Pen.EndCap;
             Cnv.Canvas.Pen.Cosmetic  := self.Pen.Cosmetic;}
           end;
           else begin
           end;
         end; // case
    end // Custom
    else
      begin
      end;
  end;

  BeginUseProfilePoints;
  try
    if Assigned(fCurvePoints) then
     begin
      if not HasTransform then
        fCurvePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
       //fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
      else
       //fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
       fCurvePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
     end;
  finally
    EndUseProfilePoints;
  end;

  if fShowDirection then
  begin
    if self.OwnerCAD <> nil then
    begin
      x := self.OwnerCAD.Viewports[0].VisualRect.Right;
      y := self.OwnerCAD.Viewports[0].VisualRect.Left;
      TmpArrowLength := (x - y) / 75;
    end else TmpArrowLength := 10;
    P0 := ProfilePoints[ProfilePoints.Count - 2];
    P1 := ProfilePoints[ProfilePoints.Count - 1];
    DrawArrows2D(MultiplyTransform2D(ModelTransform, VT), Cnv, ClipRect2D, DrawMode, P0, P1, TmpArrowLength, 0, 0);
  end;

  //if DrawMode = DRAWMODE_EDIT then
  //begin
    Cnv.Canvas.Pen.Color     := TmpPenColor;
    Cnv.Canvas.Pen.Style     := TmpPenStyle;
    Cnv.Canvas.Pen.Width     := TmpPenWidth;
    {Cnv.Canvas.Pen.Mode      := TmpPenMode;
    Cnv.Canvas.Pen.JoinStyle := TmpJoinStyle;
    Cnv.Canvas.Pen.EndCap    := TmpPenEndCap;
    Cnv.Canvas.Pen.Cosmetic  := TmpCosmetic;}
  //end;
  Cnv.Canvas.Brush.Color := TmpBrushColor;
  Cnv.Canvas.Brush.Style := TmpBrushStyle;
end;

function  TClosedCurve2D.GetBrushSource: TBrushSource;
begin
  result := fBrushSource;
end;

procedure TClosedCurve2D.SetBrushSource(ABrushSource: TBrushSource);
begin
  fBrushSource := ABrushSource;
end;

function  TClosedCurve2D.GetBrushColor: TColor;
begin
  result := fBrushColor;
end;

procedure TClosedCurve2D.SetBrushColor(ABrushColor: TColor);
begin
  fBrushColor := ABrushColor;
end;

function  TClosedCurve2D.GetBrushStyle: TBrushStyle;
begin
  result := fBrushStyle;
end;

procedure TClosedCurve2D.SetBrushStyle(ABrushStyle: TBrushStyle);
begin
  fBrushStyle := ABrushStyle;
end;

//TClosedPolyline2D
procedure   TClosedPolyline2D.Explode(ADeleteSource: boolean);
var TmpPolyline2D: TPolyline2D;  i: integer;
begin
  BeginUseProfilePoints;
  TmpPolyline2D := TPolyline2D.Create(-1, []);
  TmpPolyline2D.Assign(self);
  //for i := 0   to ProfilePoints.Count - 1 do
    //TmpPolyline2D.Points.Add(ProfilePoints[i]);
  TmpPolyline2D.Points.Add(TmpPolyline2D.Points[0]);
  TmpPolyline2D.Transform(ModelTransform);
  EndUseProfilePoints;
  TCADCmp2D(OwnerCAD).AddObject(-1, TmpPolyline2D);
end;

constructor TClosedPolyline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, Pts);
  fClosedPolyline2DInsp := TClosedPolyline2DInsp.create(self);
  fBrushSource := bsByLayer;
  fBrushColor  := clBlue;
  fBrushStyle  := bsClear;
  fDirection   := adClockwise;
end;

destructor TClosedPolyline2D.destroy;
begin
  //fBrush.Free;
  fClosedPolyline2DInsp.Free;
  inherited destroy;
end;

constructor TClosedPolyline2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpBrushSource: TBrushSource;
  TmpBrushColor:  TColor;
  TmpBrushStyle:  TBrushStyle;
begin
  inherited CreateFromStream(Stream, Version);
  fClosedPolyline2DInsp := TClosedPolyline2DInsp.create(self);

  Stream.Read(TmpBrushSource, SizeOf(TmpBrushSource));
  self.fBrushSource := TmpBrushSource;
  Stream.Read(TmpBrushColor, SizeOf(TmpBrushColor));
  self.fBrushColor := TmpBrushColor;
  Stream.Read(TmpBrushStyle, SizeOf(TmpBrushStyle));
  self.fBrushStyle := TmpBrushStyle;
end;

procedure TClosedPolyline2D.SaveToStream(const Stream: TStream);
var
  TmpBrushSource: TBrushSource;
  TmpBrushColor: TColor;
  TmpBrushStyle: TBrushStyle;
begin
  inherited SaveToStream(Stream);

  TmpBrushSource :=  self.fBrushSource;
  Stream.Write(TmpBrushSource, SizeOf(TmpBrushSource));
  TmpBrushColor := self.fBrushColor;
  Stream.Write(TmpBrushColor, SizeOf(TmpBrushColor));
  TmpBrushStyle := self.fBrushStyle;
  Stream.Write(TmpBrushStyle, SizeOf(TmpBrushStyle));
end;

procedure TClosedPolyline2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TClosedPolyline2D) then
  begin
    ProfilePoints.Copy(TClosedPolyline2D(Obj).ProfilePoints, 0, TClosedPolyline2D(Obj).ProfilePoints.Count - 1);
    fBrushSource :=  (Obj as TClosedPolyline2D).BrushSource;
    fBrushColor  :=  (Obj as TClosedPolyline2D).BrushColor;
    fBrushStyle  :=  (Obj as TClosedPolyline2D).BrushStyle;
  end else
  if (Obj is TClosedCurve2D) then
  begin
    ProfilePoints.Copy(TClosedCurve2D(Obj).ProfilePoints, 0, TClosedCurve2D(Obj).ProfilePoints.Count - 1);
    fBrushSource :=  (Obj as TClosedCurve2D).BrushSource;
    fBrushColor  :=  (Obj as TClosedCurve2D).BrushColor;
    fBrushStyle  :=  (Obj as TClosedCurve2D).BrushStyle;
  end;
end;

function TClosedPolyline2D.GetArea: TRealType;
begin
  result := ObjectArea(ProfilePoints);
end;

// =====================================================================
// TExtendedFont
// =====================================================================

procedure TExtendedFont.SetHeight(Value: Word);
begin
  LogFont.lfHeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetHeight: Word;
begin
  Result := LogFont.lfHeight;
end;

procedure TExtendedFont.SetWidth(Value: Word);
begin
  LogFont.lfWidth := Value;
  SetNewValue;
end;

function TExtendedFont.GetWidth: Word;
begin
  Result := LogFont.lfWidth;
end;

procedure TExtendedFont.SetEscapement(Value: Word);
begin
  LogFont.lfEscapement := Value;
  SetNewValue;
end;

function TExtendedFont.GetEscapement: Word;
begin
  Result := LogFont.lfEscapement;
end;

procedure TExtendedFont.SetOrientation(Value: Word);
begin
  LogFont.lfOrientation := Value;
  SetNewValue;
end;

function  TExtendedFont.GetOrientation: Word;
begin
  Result := LogFont.lfOrientation;
end;

procedure TExtendedFont.SetWeight(Value: Word);
begin
  LogFont.lfWeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetWeight: Word;
begin
  Result := LogFont.lfWeight;
end;

procedure TExtendedFont.SetItalic(Value: Byte);
begin
  LogFont.lfItalic := Value;
  SetNewValue;
end;

function TExtendedFont.GetItalic: Byte;
begin
  Result := LogFont.lfItalic;
end;

procedure TExtendedFont.SetUnderline(Value: Byte);
begin
  LogFont.lfUnderline := Value;
  SetNewValue;
end;

function TExtendedFont.GetUnderline: Byte;
begin
  Result := LogFont.lfUnderline;
end;

procedure TExtendedFont.SetStrikeOut(Value: Byte);
begin
  LogFont.lfStrikeOut := Value;
  SetNewValue;
end;

function TExtendedFont.GetStrikeOut: Byte;
begin
  Result := LogFont.lfStrikeOut;
end;

procedure TExtendedFont.SetCharSet(Value: Byte);
begin
  LogFont.lfCharSet := ARABIC_CHARSET;
  //LogFont.lfCharSet := Value;
  SetNewValue;
end;

function TExtendedFont.GetCharSet: Byte;
begin
  Result := LogFont.lfCharSet;
end;

procedure TExtendedFont.SetOutPrecision(Value: Byte);
begin
  LogFont.lfOutPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetOutPrecision: Byte;
begin
  Result := LogFont.lfOutPrecision;
end;

procedure TExtendedFont.SetClipPrecision(Value: Byte);
begin
  LogFont.lfClipPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetClipPrecision: Byte;
begin
  Result := LogFont.lfClipPrecision;
end;

procedure TExtendedFont.SetQuality(Value: Byte);
begin
  LogFont.lfQuality := Value;
  SetNewValue;
end;

function TExtendedFont.GetQuality: Byte;
begin
  Result := LogFont.lfQuality;
end;

procedure TExtendedFont.SetPicthAndFamily(Value: Byte);
begin
  LogFont.lfPitchAndFamily := Value;
  SetNewValue;
end;

function TExtendedFont.GetPicthAndFamily: Byte;
begin
  Result := LogFont.lfPitchAndFamily;
end;

procedure TExtendedFont.SetFaceName(Value: TFaceName);
var
  Cont: Byte;
begin
  for Cont := 1 to Length(Value) do
   LogFont.lfFaceName[Cont - 1] := Value[Cont];
  LogFont.lfFaceName[Length(Value)] := #0;
  SetNewValue;
end;

function TExtendedFont.GetFaceName: TFaceName;
begin
  Result := LogFont.lfFaceName;
end;

procedure TExtendedFont.SetNewValue;
var
  TmpHandle: HFONT;
begin
  TmpHandle := CreateFontIndirect(LogFont);
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, TmpHandle);
  DeleteObject(FHandle);
  FHandle := TmpHandle;
end;

procedure TExtendedFont.SetCanvas(Cnv: TCanvas);
begin
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, FCanvas.Font.Handle);
  FCanvas := Cnv;
  if Assigned(FCanvas) then SelectObject(FCanvas.Handle, FHandle);
end;

constructor TExtendedFont.Create;
begin
  inherited Create;
  GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LogFont), @LogFont);
  LogFont.lfFaceName := 'Small Font';
  FHandle := CreateFontIndirect(LogFont);
end;

procedure TExtendedFont.Assign(Obj: TExtendedFont);
begin
  if Obj = Self then
   Exit;
  LogFont := TExtendedFont(Obj).LogFont;
  SetNewValue;
end;

destructor TExtendedFont.Destroy;
begin
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, FCanvas.Font.Handle);
  DeleteObject(FHandle);
  inherited Destroy;
end;

procedure TExtendedFont.SaveToStream(Strm: TStream);
begin
  with Strm do
   Write(LogFont, SizeOf(LogFont));
end;

procedure TExtendedFont.LoadFromStream(Strm: TStream);
begin
  with Strm do
   begin
     Read(LogFont, SizeOf(LogFont));
     SetNewValue;
   end;
end;

// =====================================================================
// TPrimitive2DHandler
// =====================================================================

procedure TPrimitive2DHandler.DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer);
var
  TmpPt: TPoint2D;
  Cont: Integer;
begin
  if Sender is TPrimitive2D then
   with TPrimitive2D(Sender) do
    if not HasTransform then
     for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], VT);
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y), Width);
      end
    else
     for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], MultiplyTransform2D(ModelTransform, VT));
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y), Width);
      end;
end;

function TPrimitive2DHandler.OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  Cont: Integer;
  ResDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  if Sender is TPrimitive2D then
   with TPrimitive2D(Sender) do
    if HasTransform then
     begin
       for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(Pt, TransformPoint2D(Points[Cont], ModelTransform), Aperture, ResDist) and
           (ResDist <= Distance) then
         begin
           Result := Cont;
           Distance := ResDist;
         end;
     end
    else
     begin
       for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(Pt, Points[Cont], Aperture, ResDist) and
           (ResDist <= Distance) then
         begin
           Result := Cont;
           Distance := ResDist;
         end;
     end;
end;

// =====================================================================
// TPrimitive2D
// =====================================================================
procedure TPrimitive2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
    WritableBox := Rect2D(0, 0, 0, 0)
  else
    { Change the extension. }
    WritableBox := TransformBoundingBox2D(fPoints.Extension, ModelTransform);
end;

function TPrimitive2D.CreateVect(const Size: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D.Create(Size);
end;

constructor TPrimitive2D.Create(ID: LongInt; NPts: Integer);
begin
  inherited Create(ID);
  fPrim2DInsp := TPrim2DInsp.create(self);

  fPenSource := psByLayer;
  fPenColor  := clWhite;
  fPenStyle  := psSolid;
  fPenWidth  := 1;

  { Create the internal vector. }
  fPoints := CreateVect(NPts);
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

destructor TPrimitive2D.Destroy;
begin
  fPrim2DInsp.Free;
  if Assigned(fPoints) then
    fPoints.Free;
  inherited Destroy;
end;

procedure TPrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) then
  begin // Per default non aggiunge i punti.
    if not Assigned(fPoints) then
    begin
      fPoints := CreateVect(0);
      fPoints.GrowingEnabled := True;
      fPoints.OnChange := UpdateExtension;
    end;
    fPoints.Clear;
    LayerName   := (Obj as TPrimitive2D).LayerName;
    fPenSource  := (Obj as TPrimitive2D).PenSource;
    fPenColor   := (Obj as TPrimitive2D).PenColor;
    fPenStyle   := (Obj as TPrimitive2D).PenStyle;
    fPenWidth   := (Obj as TPrimitive2D).PenWidth;
  end;
end;

constructor TPrimitive2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var TmpWord: Word; Cont: Integer; TmpPt: TPoint2D; TmpBoolean: Boolean;
    TmpPenSource: byte;
    TmpPenColor: TColor;
    TmpPenStyle: TPenStyle;
    TmpPenWidth: word;
begin
  { Load the standard properties }
  inherited;
  fPrim2DInsp := TPrim2DInsp.create(self);
  with Stream do
   begin
     //Read(TmpBoolean, SizeOf(TmpBoolean));
     Read(TmpWord, SizeOf(TmpWord));
     fPoints := CreateVect(TmpWord);
     { Read all the points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        Read(TmpPt, SizeOf(TmpPt));
        fPoints.Points[Cont] := TmpPt;
      end;
     Read(TmpBoolean, SizeOf(TmpBoolean));
     fPoints.GrowingEnabled := TmpBoolean;
     Read(TmpPenSource, SizeOf(TmpPenSource));
     fPenSource := TPenSource(TmpPenSource);
     Read(TmpPenColor, SizeOf(TmpPenColor));
     fPenColor := TmpPenColor;
     Read(TmpPenStyle, SizeOf(TmpPenStyle));
     fPenStyle := TmpPenStyle;
     Read(TmpPenWidth, SizeOf(TmpPenWidth));
     fPenWidth := TmpPenWidth;
     //ReadPenFromStream(fPen, Stream);
   end;
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

procedure TPrimitive2D.SaveToStream(const Stream: TStream);
var TmpWord: Word; Cont: Integer; TmpPt: TPoint2D; TmpBoolean: Boolean; TmpReal: TrealType;
  TmpPenSource: byte;
  TmpPenColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenWidth: word;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     //Write(TmpBoolean, SizeOf(TmpBoolean));
     TmpWord := fPoints.Count;
     Write(TmpWord, SizeOf(TmpWord));
     { Write all points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        TmpPt := fPoints.Points[Cont];
        Write(TmpPt, SizeOf(TmpPt));
      end;
     TmpBoolean := fPoints.GrowingEnabled;
     Write(TmpBoolean, SizeOf(TmpBoolean));
     TmpPenSource := ord(fPenSource);
     Write(TmpPenSource, SizeOf(TmpPenSource));
     TmpPenColor := fPenColor;
     Write(TmpPenColor, SizeOf(TmpPenColor));
     TmpPenStyle := fPenStyle;
     Write(TmpPenStyle, SizeOf(TmpPenStyle));
     TmpPenWidth := fPenWidth;
     Write(TmpPenWidth, SizeOf(TmpPenWidth));
     //WritePenToStream(fPen, Stream);
   end;
end;

function  TPrimitive2D.GetPenSource: TPenSource;
begin
  result := fPenSource;
end;

procedure TPrimitive2D.SetPenSource(APenSource: TPenSource);
begin
  fPenSource := APenSource;
end;

function  TPrimitive2D.GetPenColor: TColor;
begin
  result := fPenColor;
end;

procedure TPrimitive2D.SetPenColor(APenColor: TColor);
begin
  fPenColor := APenColor;
end;

function  TPrimitive2D.GetPenStyle: TPenStyle;
begin
  result := fPenStyle;
end;

procedure TPrimitive2D.SetPenStyle(APenStyle: TPenStyle);
begin
  fPenStyle := APenStyle;
end;

function  TPrimitive2D.GetPenWidth: word;
begin
  result := fPenWidth;
end;

procedure TPrimitive2D.SetPenWidth(AValue: word);
begin
  fPenWidth := AValue;
end;

// =====================================================================
// TLine2D
// =====================================================================
procedure   TLine2D.InitializeAngle;
var P0, P1: TPoint2D;
begin
  P0 := TransformPoint2D(Points[0], ModelTransform);
  P1 := TransformPoint2D(Points[1], ModelTransform);
  fAngle := ArcTan2(P1.Y - P0.Y, P1.X - P0.X);
end;

constructor TLine2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
  fSimplePrim2DInsp := TSimplePrim2DInsp.create(self);
end;

destructor TLine2D.destroy;
begin
  fSimplePrim2DInsp.Free;
end;

constructor TLine2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited CreateFromStream(Stream, Version);
  fSimplePrim2DInsp := TSimplePrim2DInsp.create(self);
end;

procedure TLine2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TLine2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var x, y, TmpArrowLength: TRealType; TmpPen: TPen;  OldMode: TCopyMode;
  TmpPenColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenWidth: word;
  TmpPenMode: TPenMode;
  TmpCosmetic: boolean;
  TmpPenEndCap: TFPPenEndCap;
  TmpJoinStyle : TFPPenJoinStyle;
begin
  case fPenSource of
    psByLayer: begin
      //defaul Value is ByLayer
    end;
    psByBlock: begin
      fPenSource := psByLayer;
      exit; //not implemented.
    end;
    psCustom: begin
        //TmpPen := TPen.Create;
         TmpPenColor  := Cnv.Canvas.Pen.Color;
         TmpPenStyle  := Cnv.Canvas.Pen.Style;
         TmpPenWidth  := Cnv.Canvas.Pen.Width;
         {TmpPenMode   := Cnv.Canvas.Pen.Mode;
         TmpJoinStyle := Cnv.Canvas.Pen.JoinStyle;
         TmpPenEndCap := Cnv.Canvas.Pen.EndCap;
         TmpCosmetic  := Cnv.Canvas.Pen.Cosmetic;}
         case  DrawMode of
           DRAWMODE_EDIT: begin
             Cnv.Canvas.Pen.Color := EditPenColor;
             Cnv.Canvas.Pen.Style := EditPenStyle;
             Cnv.Canvas.Pen.Width := EditPenWidth;
           end;
           DRAWMODE_NORMAL: begin
             Cnv.Canvas.Pen.Color     := self.fPenColor;
             Cnv.Canvas.Pen.Style     := self.fPenStyle;
             Cnv.Canvas.Pen.Width     := self.fPenWidth;
             {Cnv.Canvas.Pen.Mode      := self.Pen.Mode;
             Cnv.Canvas.Pen.JoinStyle := self.Pen.JoinStyle;
             Cnv.Canvas.Pen.EndCap    := self.Pen.EndCap;
             Cnv.Canvas.Pen.Cosmetic  := self.Pen.Cosmetic;}
           end;
           else begin
           end;
         end; // case
    end // Custom
    else
      begin
      end;
  end;

  if not HasTransform then
   DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, VT)
  else
   DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, MultiplyTransform2D(ModelTransform, VT));
  if ShowDirection then
  begin
    if self.OwnerCAD <> nil then
    begin
      x := self.OwnerCAD.Viewports[0].VisualRect.Right;
      y := self.OwnerCAD.Viewports[0].VisualRect.Left;
      TmpArrowLength := (x - y) / 75;
    end else TmpArrowLength := 10;
    DrawArrows2D(MultiplyTransform2D(ModelTransform, VT), Cnv, ClipRect2D, DrawMode, Points.Points[0], Points.Points[1], TmpArrowLength, 0, 0);
  end;

  //if DrawMode = DRAWMODE_EDIT then
  //begin
    Cnv.Canvas.Pen.Color     := TmpPenColor;
    Cnv.Canvas.Pen.Style     := TmpPenStyle;
    Cnv.Canvas.Pen.Width     := TmpPenWidth;
    {Cnv.Canvas.Pen.Mode      := TmpPenMode;  //crash!!!
    Cnv.Canvas.Pen.JoinStyle := TmpJoinStyle;
    Cnv.Canvas.Pen.EndCap    := TmpPenEndCap;
    Cnv.Canvas.Pen.Cosmetic  := TmpCosmetic; }
  //end;
end;

function TLine2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform, False)]);
     Distance := MinValue([Aperture, TmpDist]);
   end;
end;

procedure TLine2D.SetLength(AValue: TRealType);
begin
  Self.Points[1] := ExtendLine(Self.Points[0], Self.Points[1],  AValue);
end;

// =====================================================================
// TOutline2D
// =====================================================================

function TOutline2D.GetHasBrush: Boolean;
begin
  BeginUseProfilePoints;
  try
   Result := (ProfilePoints.Count > 2) and
              IsSamePoint2D(ProfilePoints[0], ProfilePoints[ProfilePoints.Count - 1]);
  finally
   EndUseProfilePoints;
  end;
end;

procedure TOutline2D.BeginUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do initialization here.
end;

procedure TOutline2D.EndUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do finalization here.
end;

function TOutline2D.GetStartPoint: TPoint2D;
begin
  result := TransformPoint2D(TOutline2D(self).ProfilePoints[0], ModelTransform)
end;

function TOutline2D.GetStartPointX: TRealType;
begin
  result := GetStartPoint.X;
end;

function TOutline2D.GetStartPointY: TRealType;
begin
  result := GetStartPoint.Y
end;

procedure TOutline2D.SetStartPointX(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetStartPoint.X - AValue;
  self.Points.DisableEvents := true;
  PVectPoints2D(self.Points.PointsReference)^[0].X := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;

procedure TOutline2D.SetStartPointY(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetStartPoint.Y - AValue;
  self.Points.DisableEvents := true;
  PVectPoints2D(self.Points.PointsReference)^[0].Y := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;

function TOutline2D.GetEndPoint: TPoint2D;
begin
  result := TransformPoint2D(TOutline2D(self).ProfilePoints[TOutline2D(self).ProfilePoints.Count - 1], ModelTransform)
end;

function TOutline2D.GetEndPointX: TRealType;
begin
  result := GetEndPoint.X
end;

function TOutline2D.GetEndPointY: TRealType;
begin
  result := GetEndPoint.Y
end;

procedure TOutline2D.SetEndPointX(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetEndPoint.X - AValue;
  self.Points.DisableEvents := true;
  count := self.Points.Count;
  PVectPoints2D(self.Points.PointsReference)^[count-1].X := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;

procedure TOutline2D.SetEndPointY(AValue:TRealType);
var Delta: TRealType; i: integer;  TmpPoint2D: TPoint2D;  count: integer;
begin
  Delta := GetEndPoint.Y - AValue;
  self.Points.DisableEvents := true;
  count := self.Points.Count;
  PVectPoints2D(self.Points.PointsReference)^[count-1].Y := AValue;
  self.Points.DisableEvents := false;
  self.UpdateExtension(nil);
end;


// =====================================================================
// TPolyline2D
// =====================================================================

constructor TPolyline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1);
  Points.AddPoints(Pts);
end;

destructor TPolyline2D.destroy;
begin
  inherited;
end;

constructor TPolyline2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited CreateFromStream(Stream, Version);
  fSimplePrim2DInsp := TSimplePrim2DInsp.create(self);
end;

procedure TPolyLine2D.Explode(ADeleteSource: boolean);
var TmpLine2D: TLine2D; i: integer;
begin
  for i := 0   to ProfilePoints.Count - 2 do
  begin
    TmpLine2D := TLine2D.Create(-1, ProfilePoints[i], ProfilePoints[i+1]);
    TmpLine2D.PenSource   := PenSource;
    TmpLine2D.PenColor    := PenColor;
    TmpLine2D.PenStyle    := PenStyle;
    TmpLine2D.PenWidth    := PenWidth;
    TmpLine2D.Transform(ModelTransform);
    TCADCmp2D(OwnerCAD).AddObject(-1, TmpLine2D);
  end;
end;

procedure TPolyLine2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine2D) or (Obj is TPolyline2D) or (Obj is TPolygon2D) then
   begin
     Points.Copy(TSimplePrimitive2D(Obj).Points, 0, TSimplePrimitive2D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

procedure TPolyLine2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var P0, P1: TPoint2D; x, y, TmpArrowLength: TRealType;
  TmpPenColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenWidth: word;
  TmpPenMode: TPenMode;
  TmpCosmetic: boolean;
  TmpPenEndCap: TFPPenEndCap;
  TmpJoinStyle : TFPPenJoinStyle;
begin
  case fPenSource of
    psByLayer: begin
      //defaul Value is ByLayer
    end;
    psByBlock: begin
      fPenSource := psByLayer;
      exit; //not implemented.
    end;
    psCustom: begin
        //TmpPen := TPen.Create;
         TmpPenColor  := Cnv.Canvas.Pen.Color;
         TmpPenStyle  := Cnv.Canvas.Pen.Style;
         TmpPenWidth  := Cnv.Canvas.Pen.Width;
         {TmpPenMode   := Cnv.Canvas.Pen.Mode;
         TmpJoinStyle := Cnv.Canvas.Pen.JoinStyle;
         TmpPenEndCap := Cnv.Canvas.Pen.EndCap;
         TmpCosmetic  := Cnv.Canvas.Pen.Cosmetic;}
         case  DrawMode of
           DRAWMODE_EDIT: begin
             Cnv.Canvas.Pen.Color := EditPenColor;
             Cnv.Canvas.Pen.Style := EditPenStyle;
             Cnv.Canvas.Pen.Width := EditPenWidth;
           end;
           DRAWMODE_NORMAL: begin
             Cnv.Canvas.Pen.Color     := self.fPenColor;
             Cnv.Canvas.Pen.Style     := self.fPenStyle;
             Cnv.Canvas.Pen.Width     := self.fPenWidth;
             {Cnv.Canvas.Pen.Mode      := self.Pen.Mode;
             Cnv.Canvas.Pen.JoinStyle := self.Pen.JoinStyle;
             Cnv.Canvas.Pen.EndCap    := self.Pen.EndCap;
             Cnv.Canvas.Pen.Cosmetic  := self.Pen.Cosmetic;}
           end;
           else begin
           end;
         end; // case
    end // Custom
    else
      begin
      end;
  end;

  if not HasTransform then
   Points.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
  else
   Points.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));

  if fShowDirection then
  begin
    if self.OwnerCAD <> nil then
    begin
      x := self.OwnerCAD.Viewports[0].VisualRect.Right;
      y := self.OwnerCAD.Viewports[0].VisualRect.Left;
      TmpArrowLength := (x - y) / 75;
    end else TmpArrowLength := 10;
    P0 := ProfilePoints[ProfilePoints.Count - 2];
    P1 := ProfilePoints[ProfilePoints.Count - 1];
    DrawArrows2D(MultiplyTransform2D(ModelTransform, VT), Cnv, ClipRect2D, DrawMode, P0, P1, TmpArrowLength, 0, 0);
  end;

  //if DrawMode = DRAWMODE_EDIT then
  //begin
    Cnv.Canvas.Pen.Color     := TmpPenColor;
    Cnv.Canvas.Pen.Style     := TmpPenStyle;
    Cnv.Canvas.Pen.Width     := TmpPenWidth;
    {Cnv.Canvas.Pen.Mode     := TmpPenMode;
    Cnv.Canvas.Pen.JoinStyle := TmpJoinStyle;
    Cnv.Canvas.Pen.EndCap    := TmpPenEndCap;
    Cnv.Canvas.Pen.Cosmetic  := TmpCosmetic;}
  //end;
end;

function TPolyline2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                          var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform, False)]);
     Distance := MinValue([Aperture, TmpDist]);
   end;
end;

function TPolyline2D.GetProfilePoints: TPointsSet2D;
begin
  Result := Points;
end;

function TPolyline2D.GetNPts: Integer;
begin
  Result := Points.Count;
end;


// =====================================================================
// TPolygon2D
// =====================================================================
procedure TPolygon2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var P0, P1: TPoint2D;    x, y, TmpArrowLength: TRealType;
  TmpPenColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenWidth: word;
  TmpPenMode: TPenMode;
  TmpCosmetic: boolean;
  TmpPenEndCap: TFPPenEndCap;
  TmpJoinStyle : TFPPenJoinStyle;

  TmpBrushColor: TColor;
  TmpBrushStyle: TBrushStyle;
begin
  case self.fBrushSource of
    bsByLayer: begin
      //defaul Value is ByLayer
    end;
    bsByBlock: begin
      fBrushSource := bsByLayer;
      exit; //not implemented.
    end;
    bsCustom: begin
      TmpBrushColor := Cnv.Canvas.Brush.Color;
      TmpBrushStyle := Cnv.Canvas.Brush.Style;
      Cnv.Canvas.Brush.Color := self.BrushColor;
      Cnv.Canvas.Brush.Style := self.BrushStyle;
    end;
    else begin
      //
    end;
  end;

  case fPenSource of
    psByLayer: begin
      //defaul Value is ByLayer
    end;
    psByBlock: begin
      fPenSource := psByLayer;
      exit; //not implemented.
    end;
    psCustom: begin
        //TmpPen := TPen.Create;
         TmpPenColor  := Cnv.Canvas.Pen.Color;
         TmpPenStyle  := Cnv.Canvas.Pen.Style;
         TmpPenWidth  := Cnv.Canvas.Pen.Width;
         {TmpPenMode   := Cnv.Canvas.Pen.Mode;
         TmpJoinStyle := Cnv.Canvas.Pen.JoinStyle;
         TmpPenEndCap := Cnv.Canvas.Pen.EndCap;
         TmpCosmetic  := Cnv.Canvas.Pen.Cosmetic;}
         case  DrawMode of
           DRAWMODE_EDIT: begin
             Cnv.Canvas.Pen.Color := EditPenColor;
             Cnv.Canvas.Pen.Style := EditPenStyle;
             Cnv.Canvas.Pen.Width := EditPenWidth;
           end;
           DRAWMODE_NORMAL: begin
             Cnv.Canvas.Pen.Color     := self.fPenColor;
             Cnv.Canvas.Pen.Style     := self.fPenStyle;
             Cnv.Canvas.Pen.Width     := self.fPenWidth;
             {Cnv.Canvas.Pen.Mode      := self.Pen.Mode;
             Cnv.Canvas.Pen.JoinStyle := self.Pen.JoinStyle;
             Cnv.Canvas.Pen.EndCap    := self.Pen.EndCap;
             Cnv.Canvas.Pen.Cosmetic  := self.Pen.Cosmetic;}
           end;
           else begin
           end;
         end; // case
    end // Custom
    else
      begin
      end;
  end;
  { Draw the polygon. }

  if not HasTransform then
   Points.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
  else
   Points.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));

  if fShowDirection then
  begin
    if self.OwnerCAD <> nil then
    begin
      x := self.OwnerCAD.Viewports[0].VisualRect.Right;
      y := self.OwnerCAD.Viewports[0].VisualRect.Left;
      TmpArrowLength := (x - y) / 75;
    end else TmpArrowLength := 10;
    //P0 := ProfilePoints[ProfilePoints.Count - 2];
    //P1 := ProfilePoints[ProfilePoints.Count - 1];
    P0 := ProfilePoints[ProfilePoints.Count - 1];
    P1 := ProfilePoints[0];
    DrawArrows2D(MultiplyTransform2D(ModelTransform, VT), Cnv, ClipRect2D, DrawMode, P0, P1, TmpArrowLength, 0, 0);
  end;

  //if DrawMode = DRAWMODE_EDIT then
  //begin
    Cnv.Canvas.Pen.Color     := TmpPenColor;
    Cnv.Canvas.Pen.Style     := TmpPenStyle;
    Cnv.Canvas.Pen.Width     := TmpPenWidth;
    {Cnv.Canvas.Pen.Mode     := TmpPenMode;
    Cnv.Canvas.Pen.JoinStyle := TmpJoinStyle;
    Cnv.Canvas.Pen.EndCap    := TmpPenEndCap;
    Cnv.Canvas.Pen.Cosmetic  := TmpCosmetic;}
  //end;

  Cnv.Canvas.Brush.Color := TmpBrushColor;
  Cnv.Canvas.Brush.Style := TmpBrushStyle;
end;

function TPolygon2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                         var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) then
   begin
     //Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform)]);
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform, True)]);
     Distance := MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TCurve2D
// =====================================================================

procedure TCurve2D.SetPrimitiveSavingType(S: TPrimitiveSavingType);
begin
  if S <> fSavingType then
   begin
     fSavingType := S;
     UpdateExtension(Self);
   end;
end;

procedure TCurve2D.SetCurvePrecision(N: Word);
begin
  if fCurvePrecision <> N then
  begin
    fCurvePrecision := N;
    UpdateExtension(nil);
  end;
end;

function TCurve2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if not Assigned(fCurvePoints) then
   fCurvePoints := TPointsSet2D.Create(N)
  else
   fCurvePoints.Clear;
  Inc(fCountReference);
  fCurvePoints.GrowingEnabled := True;
  Result := Rect2D(0, 0, 0, 0);
end;

procedure TCurve2D.FreeCurvePoints;
begin
  Dec(fCountReference);
  if fCountReference <= 0 then
   begin
     fCurvePoints.Free;
     fCurvePoints := nil;
     fCountReference := 0;
   end;
end;

constructor TCurve2D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
begin
  inherited Create(ID, NPts);

  fCurvePrecision := CurvePrec;
  fCurvePoints := nil;
  fCountReference := 0;
  fSavingType := stTime;
end;

destructor TCurve2D.Destroy;
begin
  fCountReference := 0;
  FreeCurvePoints;
  inherited;
end;

procedure TCurve2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TCurve2D then
   begin
     CurvePrecision := TCurve2D(Obj).fCurvePrecision;
     SavingType := TCurve2D(Obj).fSavingType;
   end;
end;

constructor TCurve2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  with Stream do
   begin
     Read(fCurvePrecision, SizeOf(fCurvePrecision));
     Read(fSavingType, SizeOf(fSavingType));
     fCountReference := 0;
   end;
end;

procedure TCurve2D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fCurvePrecision, SizeOf(fCurvePrecision));
     Write(fSavingType, SizeOf(fSavingType));
   end;
end;

procedure TCurve2D._UpdateExtension;
begin
  if not Assigned(Points) or (Points.Count = 0) then
   Exit;
  case fSavingType of
   stSpace: begin
     WritableBox := PopulateCurvePoints(0);
     FreeCurvePoints;
   end;
   stTime: begin
     FreeCurvePoints;
     WritableBox := PopulateCurvePoints(0);
   end;
  end;
end;

procedure TCurve2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var P0, P1: TPoint2D;  x, y, TmpArrowLength: TRealType;
  TmpPenColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenWidth: word;
  TmpPenMode: TPenMode;
  TmpCosmetic: boolean;
  TmpPenEndCap: TFPPenEndCap;
  TmpJoinStyle : TFPPenJoinStyle;
begin
  case fPenSource of
    psByLayer: begin
      //defaul Value is ByLayer
    end;
    psByBlock: begin
      fPenSource := psByLayer;
      exit; //not implemented.
    end;
    psCustom: begin
        //TmpPen := TPen.Create;
         TmpPenColor  := Cnv.Canvas.Pen.Color;
         TmpPenStyle  := Cnv.Canvas.Pen.Style;
         TmpPenWidth  := Cnv.Canvas.Pen.Width;
         {TmpPenMode   := Cnv.Canvas.Pen.Mode;
         TmpJoinStyle := Cnv.Canvas.Pen.JoinStyle;
         TmpPenEndCap := Cnv.Canvas.Pen.EndCap;
         TmpCosmetic  := Cnv.Canvas.Pen.Cosmetic;}
         case  DrawMode of
           DRAWMODE_EDIT: begin
             Cnv.Canvas.Pen.Color := EditPenColor;
             Cnv.Canvas.Pen.Style := EditPenStyle;
             Cnv.Canvas.Pen.Width := EditPenWidth;
           end;
           DRAWMODE_NORMAL: begin
             Cnv.Canvas.Pen.Color     := self.fPenColor;
             Cnv.Canvas.Pen.Style     := self.fPenStyle;
             Cnv.Canvas.Pen.Width     := self.fPenWidth;
             {Cnv.Canvas.Pen.Mode      := self.Pen.Mode;
             Cnv.Canvas.Pen.JoinStyle := self.Pen.JoinStyle;
             Cnv.Canvas.Pen.EndCap    := self.Pen.EndCap;
             Cnv.Canvas.Pen.Cosmetic  := self.Pen.Cosmetic;}
           end;
           else begin
           end;
         end; // case
    end // Custom
    else
      begin
      end;
  end;

  BeginUseProfilePoints;
  try
    if Assigned(fCurvePoints) then
     begin
      if not HasTransform then
       fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
      else
       fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
     end;
  finally
    EndUseProfilePoints;
  end;

  if fShowDirection then
  begin
    if self.OwnerCAD <> nil then
    begin
      x := self.OwnerCAD.Viewports[0].VisualRect.Right;
      y := self.OwnerCAD.Viewports[0].VisualRect.Left;
      TmpArrowLength := (x - y) / 75;
    end else TmpArrowLength := 10;
    P0 := ProfilePoints[ProfilePoints.Count - 2];
    P1 := ProfilePoints[ProfilePoints.Count - 1];
    DrawArrows2D(MultiplyTransform2D(ModelTransform, VT), Cnv, ClipRect2D, DrawMode, P0, P1, TmpArrowLength, 0, 0);
  end;

  //if DrawMode = DRAWMODE_EDIT then
  //begin
    Cnv.Canvas.Pen.Color     := TmpPenColor;
    Cnv.Canvas.Pen.Style     := TmpPenStyle;
    Cnv.Canvas.Pen.Width     := TmpPenWidth;
    {Cnv.Canvas.Pen.Mode      := TmpPenMode;
    Cnv.Canvas.Pen.JoinStyle := TmpJoinStyle;
    Cnv.Canvas.Pen.EndCap    := TmpPenEndCap;
    Cnv.Canvas.Pen.Cosmetic  := TmpCosmetic;}
  //end;
end;

function TCurve2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                       var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     BeginUseProfilePoints;
     try
       if not Assigned(fCurvePoints) then
        Exit;
       Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(fCurvePoints.PointsReference, fCurvePoints.Count, Pt, TmpDist, Aperture, ModelTransform, False)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
       EndUseProfilePoints;
     end;
   end;
end;

function TCurve2D.GetProfilePoints: TPointsSet2D;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints;
end;

function TCurve2D.GetNPts: Integer;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints.Count;
end;

procedure TCurve2D.BeginUseProfilePoints;
begin
  if fSavingType = stSpace then
   WritableBox := PopulateCurvePoints(0);
end;

procedure TCurve2D.EndUseProfilePoints;
begin
  if fSavingType = stSpace then
   FreeCurvePoints;
end;

// =====================================================================
// TEllipticalArc2D
// =====================================================================
procedure TEllipticalArc2D.Inverse;
var TmpEA: TRealType;
begin
  TmpEA       := fEndAngle;
  EndAngle    := RadToDeg(fStartAngle);
  StartAngle  := RadToDeg(TmpEA);

  if fDirection  = adCounterClockwise then
    fDirection := adClockwise
  else
    fDirection := adCounterClockwise;

  UpdateExtension(self);
end;

{ Angles are in radiants. }
constructor TEllipticalArc2D.Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
begin
  inherited Create(ID, 4, 50);
  fEllipticalArc2DInsp := TEllipticalArc2DInsp.create(self);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.Add(Point2D(0, 0));
    Points.Add(Point2D(0, 0));
    fDirection := adCounterClockwise;
    StartAngle := SA;
    EndAngle := EA;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TEllipticalArc2D.GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
var P1, P0, P3: TPoint2D;  //Radius: TRealType;
begin
  P0 := CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  RX := Abs(P1.X - P0.X) / 2.0;
  RY := Abs(P1.Y - P0.Y) / 2.0;

  if Points.Count < 3 then
    Exit;

  P0 := CartesianPoint2D(Points[2]);
  P1 := CartesianPoint2D(Points[3]);
  case FDirection of
   adClockwise: begin
     SA := ArcTan2(CY - P0.Y, P0.X - CX);
     EA := ArcTan2(CY - P1.Y, P1.X - CX);
   end;
   adCounterClockwise: begin
     SA := ArcTan2(P0.Y - CY, P0.X - CX);
     EA := ArcTan2(P1.Y - CY, P1.X - CX);
   end;
  end;
end;

function  TEllipticalArc2D.GetStartAngle: TRealType;
begin
  result := RadToDeg(FStartAngle);
end;

function  TEllipticalArc2D.GetEndAngle: TRealType;
begin
  result := RadToDeg(FEndAngle);
end;

procedure TEllipticalArc2D.SetStartAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fStartAngle <> DegToRad(A) then
   begin
     fStartAngle := A;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[2] := Point2D(CX + RX * Cos(DegToRad(A)), CY + RY * Sin(DegToRad(A)));
   end;
end;

procedure TEllipticalArc2D.SetEndAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fEndAngle <> A then
   begin
     fEndAngle := A;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[3] := Point2D(CX + RX * Cos(DegToRad(A)), CY + RY * Sin(DegToRad(A)));
   end;
end;

function  TEllipticalArc2D.GetArcAngle: TRealType;
begin
  result := RadToDeg(FEndAngle - FStartAngle);
end;

procedure TEllipticalArc2D.SetArcAngle(A: TRealType);
begin
  SetEndAngle(RadToDeg(FStartAngle) + A);
  UpdateExtension(nil);
end;

function TEllipticalArc2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont, NPts: Integer;
  CX, RX, CY, RY: TRealType;
  Delta, CurrAngle: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  GetArcParams(CX, CY, RX, RY, fStartAngle, fEndAngle);
  // Calculate the effective number of points on the curve
  NPts := CurvePrecision;
  // Calculate the angular delta between two points
  if fStartAngle < fEndAngle then
    Delta := (fEndAngle - fStartAngle) / (NPts + 1)
  else
    Delta := (TWOPI - fStartAngle + fEndAngle) / (NPts + 1);
  // Create the vector of curve points
  inherited PopulateCurvePoints(NPts + 1);
   // Populate the vector of curve points
  if fDirection = adClockwise then
  begin
    CurrAngle := fStartAngle;
    //for Cont := 0 to NPts - 1 do
    for Cont := 0 to NPts do
    begin
      ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
      CurrAngle := CurrAngle + Delta
    end;
    ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY - RY * Sin(fEndAngle)));
  end else
  begin
     CurrAngle := fStartAngle;
     //for Cont := 0 to NPts - 1 do
     for Cont := 0 to NPts do
     begin
       ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY + RY * Sin(CurrAngle)));
       CurrAngle := CurrAngle + Delta
     end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY + RY * Sin(fEndAngle)));
  end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

destructor TEllipticalArc2D.destroy;
begin
  fEllipticalArc2DInsp.Free;
  inherited;
end;

procedure TEllipticalArc2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TEllipticalArc2D then
   begin
     fStartAngle := (Obj as TEllipticalArc2D).StartAngle;
     fEndAngle := (Obj as TEllipticalArc2D).EndAngle;
     fDirection := (Obj as TEllipticalArc2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end;
end;

constructor TEllipticalArc2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fEllipticalArc2DInsp := TEllipticalArc2DInsp.create(self);
  Points.DisableEvents := True;
  with Stream do
   try
     Read(FDirection, SizeOf(FDirection));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TEllipticalArc2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;


// =====================================================================
// TCircularArc2D
// =====================================================================
procedure TCircularArc2D.Inverse;
var TmpEA: TRealType;
begin
  TmpEA       := fEndAngle;
  EndAngle    := RadToDeg(fStartAngle);
  StartAngle  := RadToDeg(TmpEA);

  if fDirection  = adCounterClockwise then
    fDirection := adClockwise
  else
    fDirection := adCounterClockwise;

  UpdateExtension(self);
end;

{ Angles are in radiants. }
constructor TCircularArc2D.Create(ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
begin
  inherited Create(ID, 4, 50);
  fCircularArc2DInsp := TCircularArc2DInsp.create(self);
  Points.DisableEvents := True;
  try
    fPoints.Add(CP);
    fPoints.Add(Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA)));
    fPoints.Add(Point2D(CP.X + R * Cos(EA), CP.Y + R * Sin(EA)));
    FStartAngle := SA;
    FEndAngle := EA;
    fRadius := R;
    FDirection := adCounterClockwise;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TCircularArc2D.GetArcParams(var CX, CY, R, SA, EA: TRealType);
const TwoPi = 2 * pi;
var P0, P1, P2: TPoint2D;

  function NormalizeAngle(Angle: TRealType): TRealType;
  begin
    if Angle < 0 then
      Result := Angle + TwoPi
    else if Angle >= TwoPi then
      Result := Angle - TwoPi
    else
      Result := Angle;
  end;

begin
  P0 := CartesianPoint2D(fPoints[0]);
  P1 := CartesianPoint2D(fPoints[1]);
  P2 := CartesianPoint2D(fPoints[2]);
  CX := P0.X;
  CY := P0.Y;
  R  := PointDistance2D(P0, P1);

  if FDirection = adCounterClockwise then
  begin
    SA := ArcTan2(P1.Y - CY, P1.X - CX);
    EA := ArcTan2(P2.Y - CY, P2.X - CX);
  end else
  begin  //Clockwise
    SA := ArcTan2(P2.Y - CY, P2.X - CX);
    EA := ArcTan2(P1.Y - CY, P1.X - CX);
  end;

  SA := NormalizeAngle(SA);
  EA := NormalizeAngle(EA);

  {if  SA < 0 then SA := (2*pi) - abs(SA);
  if  SA = 2*pi then SA := 0;
  if  SA = (2*pi) then SA := 0;

  if  EA < 0 then EA := (2*pi) - abs(EA);
  if  EA = 2*pi then EA := 0;
  if  EA = (2*pi) then EA := 0;  }
end;

function  TCircularArc2D.GetStartAngle: TrealType;
begin
  result := RadToDeg(FStartAngle);
end;

function  TCircularArc2D.GetEndAngle: TrealType;
begin
  result := RadToDeg(FEndAngle);
end;

procedure TCircularArc2D.SetStartAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FStartAngle <> DegToRad(A) then
  begin
    FStartAngle := DegToRad(A);
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[1] := Point2D(CX + R * Cos(DegToRad(A)), CY + R * Sin(DegToRad(A)));
  end;
end;

procedure TCircularArc2D.SetEndAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FEndAngle <> DegToRad(A) then
  begin
    FEndAngle := DegToRad(A);
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[2] := Point2D(CX + R * Cos(DegToRad(A)), CY + R * Sin(DegToRad(A)));
  end;
end;

function  TCircularArc2D.GetArcAngle: TRealType;
begin
  result := RadToDeg(FEndAngle - FStartAngle);
end;

procedure TCircularArc2D.SetArcAngle(A: TRealType);
begin
  SetEndAngle(RadToDeg(FStartAngle) + A);
  UpdateExtension(nil);
end;

procedure TCircularArc2D.SetRadius(ARadius: TRealType);
begin
  if ARadius <> fRadius then
  begin
    if ARadius = 0 then ARadius := 0.001;
    fRadius := ARadius;
    fPoints[1] := ExtendLine(fPoints[0], fPoints[1], fRadius);
    fPoints[2] := ExtendLine(fPoints[0], fPoints[2], fRadius);
  end;
end;

procedure TCircularArc2D.GetArcPoints(PP: TPointsSet2D; NPts: Word);
var  ArcAngle: TRealType;
  Cont: Integer;
  CX, CY, R: TRealType;
  Delta, CurrAngle: TRealType;
begin
  GetArcParams(CX, CY, R, FStartAngle, FEndAngle);
  fRadius := R;
  // Calcola il delta angolare tra due punti
  if FStartAngle < FEndAngle then
    Delta := (FEndAngle - FStartAngle) / (NPts - 1)
  else
    Delta := (TWOPI - FStartAngle + FEndAngle) / (NPts - 1);
  // Crea il vettore curvilineo.
  // Popola il vettore curvilineo.
  if FDirection = adClockwise then
  begin
    ArcAngle    := FEndAngle - FStartAngle;
    FEndAngle   := FEndAngle   + ArcAngle;
    FStartAngle := FStartAngle + ArcAngle;
  end;
  CurrAngle := FStartAngle;
  for Cont := 1 to NPts do
  begin
    ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
    if FDirection = adCounterClockwise then
      CurrAngle := CurrAngle + Delta
    else
      CurrAngle := CurrAngle - Delta;
  end;
  //Make Sector
  //ProfilePoints.Add(Point2D(CX, CY));
  //ProfilePoints.Add(ProfilePoints[0]);

  //Make Segment
  //ProfilePoints.Add(ProfilePoints[0]); //close Arc
end;

{procedure TCircularArc2D.GetArcPoints(PP: TPointsSet2D; NPts: Word);
var
  CX, CY, R, SA, EA: TRealType;
  Delta, CurrAngle: TRealType;
  Cont: Integer;
begin
  // Holen Sie sich die Bogenparameter
  GetArcParams(CX, CY, R, SA, EA);
  fRadius := R;

  // Berechne das Delta zwischen zwei Punkten
  if FDirection = CounterClockwise then
  begin
    if SA < EA then
      Delta := (EA - SA) / (NPts - 1)
    else
      Delta := (2 * Pi - SA + EA) / (NPts - 1);
    CurrAngle := SA;
  end
  else
  begin
    if SA > EA then
      Delta := (SA - EA) / (NPts - 1)
    else
      Delta := (2 * Pi - EA + SA) / (NPts - 1);
    CurrAngle := SA;
  end;

  // F�gen Sie Punkte zum Bogen hinzu
  for Cont := 0 to NPts - 1 do
  begin
    PP.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
    if FDirection = CounterClockwise then
      CurrAngle := CurrAngle + Delta
    else
      CurrAngle := CurrAngle - Delta;
  end;

  // Optional: Schlie�en Sie den Bogen
  // PP.Add(PP[0]);
end; }

function TCircularArc2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  Result := inherited PopulateCurvePoints(CurvePrecision);
  GetArcPoints(ProfilePoints, CurvePrecision);
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

destructor TCircularArc2D.destroy;
begin
  fCircularArc2DInsp.Free;
  inherited;
end;

procedure TCircularArc2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TEllipticalArc2D then
   begin
     fStartAngle := (Obj as TEllipticalArc2D).StartAngle;
     fEndAngle := (Obj as TEllipticalArc2D).EndAngle;
     fDirection := (Obj as TEllipticalArc2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end
  else if  Obj is TCircularArc2D then
  begin
    fStartAngle := (Obj as TCircularArc2D).StartAngle;
    fEndAngle := (Obj as TCircularArc2D).EndAngle;
    fDirection := (Obj as TCircularArc2D).Direction;
    fShowDirection := (Obj as TCircularArc2D).fShowDirection;
    Points.Copy(TPrimitive2D(Obj).Points, 0, 2);
    Points.GrowingEnabled := False;
  end;
end;

constructor TCircularArc2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fCircularArc2DInsp := TCircularArc2DInsp.create(self);
  Points.DisableEvents := True;
  with Stream do
   try
     //Read(FDirection, SizeOf(FDirection));
     Read(fRadius, SizeOf(fRadius));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TCircularArc2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   //Write(fDirection, SizeOf(FDirection));
    Write(fRadius, SizeOf(fRadius));
end;

function  TCircularArc2D.GetCenterPoint: TPoint2D;
begin
  result := self.Points[0];
end;

procedure TCircularArc2D.SetCenterPoint(APoint2D: TPoint2D);
begin
  Points[0] := APoint2D;
end;

function  TCircularArc2D.GetCenterPointX: TRealType;
begin
  result := self.Points[0].X;
end;

procedure TCircularArc2D.SetCenterPointX(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt := Point2D(AValue, Points[0].Y);
  ToPt := WorldToObject(ToPt);
  MoveTo(ToPt, DragPt);
end;

function  TCircularArc2D.GetCenterPointY: TRealType;
begin
  result := self.Points[0].Y;
end;

procedure TCircularArc2D.SetCenterPointY(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt := Point2D(Points[0].X, AValue);
  ToPt := WorldToObject(ToPt);
  MoveTo(ToPt, DragPt);
end;

//=====================================================================
// TSector2D
//=====================================================================
procedure TSector2D.GetArcParams(var CX, CY, R, SA, EA: TRealType);
const TwoPi = 2 * pi;
var P0, P1, P2: TPoint2D;

  function NormalizeAngle(Angle: TRealType): TRealType;
  begin
    if Angle < 0 then
      Result := Angle + TwoPi
    else if Angle >= TwoPi then
      Result := Angle - TwoPi
    else
      Result := Angle;
  end;

begin
  P0 := CartesianPoint2D(fPoints[0]);
  P1 := CartesianPoint2D(fPoints[1]);
  P2 := CartesianPoint2D(fPoints[2]);
  CX := P0.X;
  CY := P0.Y;
  R  := PointDistance2D(P0, P1);

  if FDirection = adCounterClockwise then
  begin
    SA := ArcTan2(P1.Y - CY, P1.X - CX);
    EA := ArcTan2(P2.Y - CY, P2.X - CX);
  end else
  begin  //Clockwise
    SA := ArcTan2(P2.Y - CY, P2.X - CX);
    EA := ArcTan2(P1.Y - CY, P1.X - CX);
  end;

  SA := NormalizeAngle(SA);
  EA := NormalizeAngle(EA);

  {if  SA < 0 then SA := (2*pi) - abs(SA);
  if  SA = 2*pi then SA := 0;
  if  SA = (2*pi) then SA := 0;

  if  EA < 0 then EA := (2*pi) - abs(EA);
  if  EA = 2*pi then EA := 0;
  if  EA = (2*pi) then EA := 0;  }
end;

function  TSector2D.GetStartAngle: TRealType;
begin
  result := RadToDeg(FStartAngle);
end;

function  TSector2D.GetEndAngle: TrealType;
begin
  result := RadToDeg(FEndAngle);
end;

procedure TSector2D.SetStartAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FStartAngle <> DegToRad(A) then
  begin
    FStartAngle := DegToRad(A);
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[1] := Point2D(CX + R * Cos(DegToRad(A)), CY + R * Sin(DegToRad(A)));
  end;
end;

procedure TSector2D.SetEndAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FEndAngle <> DegToRad(A) then
  begin
    FEndAngle := DegToRad(A);
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[2] := Point2D(CX + R * Cos(DegToRad(A)), CY + R * Sin(DegToRad(A)));
  end;
end;

function  TSector2D.GetArcAngle: TRealType;
begin
  result := RadToDeg(FEndAngle - FStartAngle);
end;

procedure TSector2D.SetArcAngle(A: TRealType);
begin
  SetEndAngle(RadToDeg(FStartAngle) + A);
  UpdateExtension(nil);
end;

procedure TSector2D.SetRadius(ARadius: TRealType);
begin
  if ARadius <> fRadius then
  begin
    if ARadius = 0 then ARadius := 0.001;
    fRadius := ARadius;
    fPoints[1] := ExtendLine(fPoints[0], fPoints[1], fRadius);
    fPoints[2] := ExtendLine(fPoints[0], fPoints[2], fRadius);
  end;
end;

procedure TSector2D.GetArcPoints(PP: TPointsSet2D; NPts: Word);
var  ArcAngle: TRealType;
  Cont: Integer;
  CX, CY, R: TRealType;
  Delta, CurrAngle: TRealType;
begin
  GetArcParams(CX, CY, R, FStartAngle, FEndAngle);
  fRadius := R;
  // Calcola il delta angolare tra due punti
  if FStartAngle < FEndAngle then
    Delta := (FEndAngle - FStartAngle) / (NPts - 1)
  else
    Delta := (TWOPI - FStartAngle + FEndAngle) / (NPts - 1);
  // Crea il vettore curvilineo.
  // Popola il vettore curvilineo.
  if FDirection = adClockwise then
  begin
    ArcAngle    := FEndAngle - FStartAngle;
    FEndAngle   := FEndAngle   + ArcAngle;
    FStartAngle := FStartAngle + ArcAngle;
  end;
  CurrAngle := FStartAngle;
  for Cont := 1 to NPts do
  begin
    ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
    if FDirection = adCounterClockwise then
      CurrAngle := CurrAngle + Delta
    else
      CurrAngle := CurrAngle - Delta;
  end;
  //Make Sector
  ProfilePoints.Add(Point2D(CX, CY));
  ProfilePoints.Add(ProfilePoints[0]);

  //Make Segment
  //ProfilePoints.Add(ProfilePoints[0]); //close Arc
end;

function TSector2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  Result := inherited PopulateCurvePoints(CurvePrecision);
  GetArcPoints(ProfilePoints, CurvePrecision);
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

procedure TSector2D.Inverse;
var TmpEA: TRealType;
begin
  TmpEA       := fEndAngle;
  EndAngle    := RadToDeg(fStartAngle);
  StartAngle  := RadToDeg(TmpEA);

  if fDirection  = adCounterClockwise then
    fDirection := adClockwise
  else
    fDirection := adCounterClockwise;

  UpdateExtension(self);
end;

procedure TSector2D.Explode(ADeleteSource: boolean);
begin
  inherited Explode(ADeleteSource);
end;

{ Angles are in radiants. }
constructor TSector2D.Create(ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
begin
  inherited Create(ID, 3, 50);

  fSector2DInsp := TSector2DInsp.create(self);

  Points.DisableEvents := True;
  try
    fPoints.Add(CP);
    fPoints.Add(Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA)));
    fPoints.Add(Point2D(CP.X + R * Cos(EA), CP.Y + R * Sin(EA)));
    FStartAngle := SA;
    FEndAngle := EA;
    fRadius := R;
    FDirection := adCounterClockwise;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TSector2D.destroy;
begin
  fSector2DInsp.Free;
  inherited;
end;

procedure TSector2D.Assign(const Obj: TGraphicObject);
var i, x: integer;
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);

  if (Obj is TSector2D) then
  begin
    UpdateExtension(Self);
    self.BeginUseProfilePoints;
    self.ProfilePoints.DisableEvents := true;
    ProfilePoints.Copy(TSector2D(Obj).ProfilePoints, 0, TSector2D(Obj).ProfilePoints.Count - 1);
    self.ProfilePoints.DisableEvents := false;
    self.EndUseProfilePoints;
    UpdateExtension(Self);
  end else

  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TEllipticalArc2D then
   begin
     fStartAngle := (Obj as TEllipticalArc2D).StartAngle;
     fEndAngle := (Obj as TEllipticalArc2D).EndAngle;
     fDirection := (Obj as TEllipticalArc2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end
  else if  Obj is TCircularArc2D then
  begin
    fStartAngle := (Obj as TCircularArc2D).StartAngle;
    fEndAngle := (Obj as TCircularArc2D).EndAngle;
    fDirection := (Obj as TCircularArc2D).Direction;
    fShowDirection := (Obj as TCircularArc2D).fShowDirection;
    Points.Copy(TPrimitive2D(Obj).Points, 0, 2);
    Points.GrowingEnabled := False;
  end;
end;

constructor TSector2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fSector2DInsp := TSector2DInsp.create(self);
  Points.DisableEvents := True;
  with Stream do
   try
     //Read(FDirection, SizeOf(FDirection));
     Read(fRadius, SizeOf(fRadius));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TSector2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   //Write(fDirection, SizeOf(FDirection));
    Write(fRadius, SizeOf(fRadius));
end;

function  TSector2D.GetCenterPoint: TPoint2D;
begin
  result := self.Points[0];
end;

procedure TSector2D.SetCenterPoint(APoint2D: TPoint2D);
begin
  Points[0] := APoint2D;
end;

function  TSector2D.GetCenterPointX: TRealType;
begin
  result := self.Points[0].X;
end;

procedure TSector2D.SetCenterPointX(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt := Point2D(AValue, Points[0].Y);
  ToPt := WorldToObject(ToPt);
  MoveTo(ToPt, DragPt);
end;

function  TSector2D.GetCenterPointY: TRealType;
begin
  result := self.Points[0].Y;
end;

procedure TSector2D.SetCenterPointY(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt := Point2D(Points[0].X, AValue);
  ToPt := WorldToObject(ToPt);
  MoveTo(ToPt, DragPt);
end;

//=====================================================================
// TSegment2D
//=====================================================================
procedure TSegment2D.GetArcParams(var CX, CY, R, SA, EA: TRealType);
const TwoPi = 2 * pi;
var P0, P1, P2: TPoint2D;

  function NormalizeAngle(Angle: TRealType): TRealType;
  begin
    if Angle < 0 then
      Result := Angle + TwoPi
    else if Angle >= TwoPi then
      Result := Angle - TwoPi
    else
      Result := Angle;
  end;

begin
  P0 := CartesianPoint2D(fPoints[0]);
  P1 := CartesianPoint2D(fPoints[1]);
  P2 := CartesianPoint2D(fPoints[2]);
  CX := P0.X;
  CY := P0.Y;
  R  := PointDistance2D(P0, P1);

  if FDirection = adCounterClockwise then
  begin
    SA := ArcTan2(P1.Y - CY, P1.X - CX);
    EA := ArcTan2(P2.Y - CY, P2.X - CX);
  end else
  begin  //Clockwise
    SA := ArcTan2(P2.Y - CY, P2.X - CX);
    EA := ArcTan2(P1.Y - CY, P1.X - CX);
  end;

  SA := NormalizeAngle(SA);
  EA := NormalizeAngle(EA);

  {if  SA < 0 then SA := (2*pi) - abs(SA);
  if  SA = 2*pi then SA := 0;
  if  SA = (2*pi) then SA := 0;

  if  EA < 0 then EA := (2*pi) - abs(EA);
  if  EA = 2*pi then EA := 0;
  if  EA = (2*pi) then EA := 0;  }
end;

function  TSegment2D.GetStartAngle: TRealType;
begin
  result := RadToDeg(FStartAngle);
end;

function  TSegment2D.GetEndAngle: TrealType;
begin
  result := RadToDeg(FEndAngle);
end;

procedure TSegment2D.SetStartAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FStartAngle <> DegToRad(A) then
  begin
    FStartAngle := DegToRad(A);
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[1] := Point2D(CX + R * Cos(DegToRad(A)), CY + R * Sin(DegToRad(A)));
  end;
end;

procedure TSegment2D.SetEndAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FEndAngle <> DegToRad(A) then
  begin
    FEndAngle := DegToRad(A);
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[2] := Point2D(CX + R * Cos(DegToRad(A)), CY + R * Sin(DegToRad(A)));
  end;
end;

function  TSegment2D.GetArcAngle: TRealType;
begin
  result := RadToDeg(FEndAngle - FStartAngle);
end;

procedure TSegment2D.SetArcAngle(A: TRealType);
begin
  SetEndAngle(RadToDeg(FStartAngle) + A);
  UpdateExtension(nil);
end;

procedure TSegment2D.SetRadius(ARadius: TRealType);
begin
  if ARadius <> fRadius then
  begin
    if ARadius = 0 then ARadius := 0.001;
    fRadius := ARadius;
    fPoints[1] := ExtendLine(fPoints[0], fPoints[1], fRadius);
    fPoints[2] := ExtendLine(fPoints[0], fPoints[2], fRadius);
  end;
end;

procedure TSegment2D.GetArcPoints(PP: TPointsSet2D; NPts: Word);
var  ArcAngle: TRealType;
  Cont: Integer;
  CX, CY, R: TRealType;
  Delta, CurrAngle: TRealType;
begin
  GetArcParams(CX, CY, R, FStartAngle, FEndAngle);
  fRadius := R;
  // Calcola il delta angolare tra due punti
  if FStartAngle < FEndAngle then
    Delta := (FEndAngle - FStartAngle) / (NPts - 1)
  else
    Delta := (TWOPI - FStartAngle + FEndAngle) / (NPts - 1);
  // Crea il vettore curvilineo.
  // Popola il vettore curvilineo.
  if FDirection = adClockwise then
  begin
    ArcAngle    := FEndAngle - FStartAngle;
    FEndAngle   := FEndAngle   + ArcAngle;
    FStartAngle := FStartAngle + ArcAngle;
  end;
  CurrAngle := FStartAngle;
  for Cont := 1 to NPts do
  begin
    ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
    if FDirection = adCounterClockwise then
      CurrAngle := CurrAngle + Delta
    else
      CurrAngle := CurrAngle - Delta;
  end;
  //Make Sector
  //ProfilePoints.Add(Point2D(CX, CY));
  //ProfilePoints.Add(ProfilePoints[0]);

  //Make Segment
  ProfilePoints.Add(ProfilePoints[0]); //close Arc
end;

function TSegment2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  Result := inherited PopulateCurvePoints(CurvePrecision);
  GetArcPoints(ProfilePoints, CurvePrecision);
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

procedure TSegment2D.Inverse;
var TmpEA: TRealType;
begin
  TmpEA       := fEndAngle;
  EndAngle    := RadToDeg(fStartAngle);
  StartAngle  := RadToDeg(TmpEA);

  if fDirection  = adCounterClockwise then
    fDirection := adClockwise
  else
    fDirection := adCounterClockwise;

  UpdateExtension(self);
end;

procedure TSegment2D.Explode(ADeleteSource: boolean);
begin
  inherited Explode(ADeleteSource);
end;

{ Angles are in radiants. }
constructor TSegment2D.Create(ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
begin
  inherited Create(ID, 4, 50);
  fSegment2DInsp := TSegment2DInsp.create(self);
  Points.DisableEvents := True;
  try
    fPoints.Add(CP);
    fPoints.Add(Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA)));
    fPoints.Add(Point2D(CP.X + R * Cos(EA), CP.Y + R * Sin(EA)));
    FStartAngle := SA;
    FEndAngle := EA;
    fRadius := R;
    FDirection := adCounterClockwise;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TSegment2D.destroy;
begin
  fSegment2DInsp.Free;
  inherited;
end;

procedure TSegment2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TEllipticalArc2D then
   begin
     fStartAngle := (Obj as TEllipticalArc2D).StartAngle;
     fEndAngle := (Obj as TEllipticalArc2D).EndAngle;
     fDirection := (Obj as TEllipticalArc2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end
  else if  Obj is TCircularArc2D then
  begin
    fStartAngle := (Obj as TCircularArc2D).StartAngle;
    fEndAngle := (Obj as TCircularArc2D).EndAngle;
    fDirection := (Obj as TCircularArc2D).Direction;
    fShowDirection := (Obj as TCircularArc2D).fShowDirection;
    Points.Copy(TPrimitive2D(Obj).Points, 0, 2);
    Points.GrowingEnabled := False;
  end;
end;

constructor TSegment2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fSegment2DInsp := TSegment2DInsp.create(self);
  Points.DisableEvents := True;
  with Stream do
   try
     //Read(FDirection, SizeOf(FDirection));
     Read(fRadius, SizeOf(fRadius));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TSegment2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   //Write(fDirection, SizeOf(FDirection));
    Write(fRadius, SizeOf(fRadius));
end;

function  TSegment2D.GetCenterPoint: TPoint2D;
begin
  result := self.Points[0];
end;

procedure TSegment2D.SetCenterPoint(APoint2D: TPoint2D);
begin
  Points[0] := APoint2D;
end;

function  TSegment2D.GetCenterPointX: TRealType;
begin
  result := self.Points[0].X;
end;

procedure TSegment2D.SetCenterPointX(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt := Point2D(AValue, Points[0].Y);
  ToPt := WorldToObject(ToPt);
  MoveTo(ToPt, DragPt);
end;

function  TSegment2D.GetCenterPointY: TRealType;
begin
  result := self.Points[0].Y;
end;

procedure TSegment2D.SetCenterPointY(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt := Point2D(Points[0].X, AValue);
  ToPt := WorldToObject(ToPt);
  MoveTo(ToPt, DragPt);
end;

// =====================================================================
// TCircle2D
// =====================================================================
constructor TCircle2D.Create(ID: LongInt; const P1: TPoint2D; R : TRealType);
var P2 : TPoint2d;
begin
  inherited Create(ID, 2, 50);
  fCircle2DInsp := TCircle2DInsp.create(self);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    P2 := P1; P2.X := P2.X + R;
    Points.Add(P2);
    fStartAngle := 0;
    fDirection := adCounterClockwise;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

constructor TCircle2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fCircle2DInsp := TCircle2DInsp.create(self);
end;

destructor TCircle2D.destroy;
begin
  fCircle2DInsp.Free;
  inherited;
end;

procedure OutText2D(ACADViewport2D: TCADViewport2D; APoint2D: TPoint2D; AText: string;  CntlPoints: boolean);
var TmpText2D: TText2D;
begin
  TmpText2D := TText2D.Create(-1, Rect2D(APoint2D.X, APoint2D.Y, APoint2D.X+100, APoint2D.Y+100), 10, AText);
  //TmpText2D.Text   := AText;
  TmpText2D.Left   := APoint2D.X;
  TmpText2D.Top    := APoint2D.Y;
  TmpText2D.Right  := TmpText2D.Left;;
  TmpText2D.Bottom := TmpText2D.Top;

  //TmpText2D
  //ACADViewport2D.Canvas.TextOut(round(APoint2D.X), round(APoint2D.y), AText);
  ACADViewport2D.DrawObject2D(TmpText2D, CntlPoints);
end;

procedure TCircle2D.GetCircleParams(var CX, CY, R, SA: TRealType);
var P1, P0: TPoint2D;  hStr: string; hText: TText2D;
begin
  P0 := CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  R  := SQRT(SQR(P1.X - P0.X) + SQR(P1.Y - P0.Y)) ;
  CX := P0.X;
  CY := P0.Y;
  if FDirection = adCounterClockwise then
    //SA := AngleFromPoints2D(P0, P1)
    SA := ArcTan2(P1.Y - CY, P1.X - CX)
  else
    //SA := AngleFromPoints2D(P1, P0);
    SA := ArcTan2(CY - P1.Y, P1.X - CX);

  hStr := 'Angle = ' + FloatToStr(RadToDeg(SA));
  //if OwnerCAd <> nil then
    //OutText2D(TCADViewport2D(self.OwnerCAD.Viewports[0]), P1, hStr, false);
end;

function  TCircle2D.GetStartAngle: TRealType;
begin
  result := RadToDeg(fStartAngle);
end;

procedure TCircle2D.SetStartAngle(AValue: TRealType);
var CX, CY, R, SA, EA: TRealType;
begin
  if fStartAngle <> DegToRad(AValue) then
  begin
    fStartAngle := DegToRad(AValue);
    GetCircleParams(CX, CY, R, fStartAngle);
    fPoints[1] := Point2D(CX + R * Cos( DegToRad(AValue) ), CY + R * Sin( DegToRad(AValue) ));
  end;
end;

function TCircle2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
  Delta, CurrAngle, CX, CY, R, testang: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);

  GetCircleParams(CX, CY, R, fStartAngle);
  fRadius := R;
  Delta := TWOPI / CurvePrecision;
  CurrAngle := fStartAngle;
  for Cont := 1 to CurvePrecision  do
  begin
    if fDirection = adClockwise then
    begin
      ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY - R * Sin(CurrAngle)));
      CurrAngle := CurrAngle + Delta
    end else
    begin
      ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
      CurrAngle := CurrAngle + Delta
    end;
  end;
  ProfilePoints.Add(ProfilePoints[0]);
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

procedure TCircle2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TCircle2D) or (Obj is TFrame2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TCircle2D.SetRadius(AValue: TRealType);
begin
  if AValue <> fRadius then
  begin
    if AValue = 0 then AValue := 0.001;
    fRadius := AValue;
    fPoints[1] := ExtendLine(fPoints[0], fPoints[1], AValue);
  end;
end;

function  TCircle2D.GetCenterPoint: TPoint2D;
begin
  result := Points[0];
end;

procedure TCircle2D.SetCenterPoint(APoint2D: TPoint2D);
var TmpPoint2D: TPoint2D;
begin
  self.Points.Delete(0);
  self.Points.Insert(0, APoint2D);
end;

function  TCircle2D.GetCenterPointX: TRealType;
begin
  result := Points[0].X;
end;

function  TCircle2D.GetCenterPointY: TRealType;
begin
  result := Points[0].Y;
end;

procedure TCircle2D.SetCenterPointX(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt   := Point2D(AValue,Points[0].Y);
  self.MoveTo(ToPt, DragPt);
end;

procedure TCircle2D.SetCenterPointY(AValue: TRealType);
var ToPt, DragPt: TPoint2D;
begin
  DragPt := Points[0];
  ToPt   := Point2D(Points[0].X, AValue);
  self.MoveTo(ToPt, DragPt);
end;

// =====================================================================
// TFrame2D
// =====================================================================
procedure  TFrame2D.SetStartCorner(AValue: TFrameStartCorner);
begin
  fStartCorner := AValue;
  UpdateExtension(self);
end;

{procedure  TFrame2D.SetDirection(ADirection: TArcDirection);
begin
  fDirection := ADirection;
  UpdateExtension(self);
end; }

procedure  TFrame2D.SetChamfered(AValue: boolean);
begin
  fChamfered := AValue;
  UpdateExtension(self);
end;

procedure  TFrame2D.SetChamferValue(AValue:  TRealType);
begin
  fChamferValue := AValue;
  UpdateExtension(self);
end;

function TFrame2D.GetWidth: TRealType;
begin
  result := Abs(Points[1].X - Points[0].X);
end;

procedure TFrame2D.SetWidth(AValue: TRealType);
var P1: TPoint2D;
begin
  P1 := self.Points[1];
  P1.X := self.Points[0].X + AValue;
  self.Points[1] := P1;
end;

function  TFrame2D.GetHeight: TRealType;
begin
  result := Abs(Points[1].Y - Points[0].Y);
end;

procedure TFrame2D.SetHeight(AValue: TRealType);
var P0: TPoint2D;
begin
  P0 := self.Points[0];
  P0.Y := self.Points[1].Y + AValue;
  self.Points[0] := P0;
end;

function TFrame2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);
  if FDirection = adCounterClockwise then
  begin
    case fStartCorner of
      fsLeftTop: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
        end else
        begin
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[0]);
        end;
      end;
      fsLeftBottom: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
        end else
        begin
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
        end;
      end;
      fsRightBottom: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
        end else
        begin
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[1]);
        end;
      end;
      fsRightTop: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
        end else
        begin
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
        end;
      end;
    end;
  end else
  begin  //Clockwise
    case fStartCorner of
      fsLeftTop: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
        end else
        begin
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[0]);
        end;
      end;
      fsLeftBottom: begin
        if fChamfered then
        begin
         ProfilePoints.Add(Point2D(Points[0].X +  fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
        end else
        begin
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
        end;
      end;
      fsRightBottom: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
        end else
        begin
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[1]);
        end;
      end;
      fsRightTop: begin
        if fChamfered then
        begin
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[0].Y));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[0].X + fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[1].Y));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[1].Y+fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X,     Points[0].Y-fChamferValue));
          ProfilePoints.Add(Point2D(Points[1].X - fChamferValue, Points[0].Y));
        end else
        begin
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
          ProfilePoints.Add(Points[1]);
          ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
          ProfilePoints.Add(Points[0]);
          ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
        end;
      end;
    end;
  end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;


procedure   TFrame2D.InitializeAngle;
var P0, P1: TPoint2D;
begin
  P0 := TransformPoint2D(Points[0], ModelTransform);
  P1 := TransformPoint2D(Point2D(Points[1].X, Points[0].Y), ModelTransform);
  fAngle := ArcTan2(P1.Y - P0.Y, P1.X - P0.X);
end;

constructor TFrame2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2, 50);
  fFrame2DInsp := TFrame2DInsp.create(self);
  Points.DisableEvents := True;
  try
    Points.Add(P2);
    Points.Add(P1);
    Points.GrowingEnabled := False;
    Width := Points[1].X - Points[0].X;
    fStartCorner := fsRightBottom;
    fChamferValue := 10;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TFrame2D.destroy;
begin
  fFrame2DInsp.Free;
  inherited;
end;

procedure TFrame2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TFrame2D) then
   begin
     if (Obj is TFrame2D) then   //added
      begin
        fChamfered     := (Obj as TFrame2D).Chamfered;
        fChamferValue  := (Obj as TFrame2D).ChamferValue;
        fStartCorner   := (Obj as TFrame2D).StartCorner;
        fAngle         := (Obj as TFrame2D).fAngle;
      end;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TFrame2D.SaveToStream(const Stream: TStream);
begin
  inherited;
  Stream.Write(fStartCorner, SizeOf(fStartCorner));
  Stream.Write(fChamfered, SizeOf(fChamfered));
  Stream.Write(fChamferValue, SizeOf(fChamferValue));
end;

constructor TFrame2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  fFrame2DInsp := TFrame2DInsp.create(self);
  Stream.Read(fStartCorner, SizeOf(fStartCorner));
  Stream.Read(fChamfered, SizeOf(fChamfered));
  Stream.Read(fChamferValue, SizeOf(fChamferValue));
end;

procedure TFrame2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  inherited Draw(VT, Cnv, ClipRect2D, DrawMode);
end;

// =====================================================================
// TRectangle2D
// =====================================================================

procedure TRectangle2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
   if not HasTransform then
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
   else
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
  finally
   EndUseProfilePoints;
  end;
end;

function TRectangle2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                           var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     { Consider all segments in the arc. }
     BeginUseProfilePoints;
     try
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(ProfilePoints.PointsReference, ProfilePoints.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
      EndUseProfilePoints;
     end;
   end;
end;

// =====================================================================
// TEllipse2D
// =====================================================================
procedure   TEllipse2D.InitializeAngle;
var P0, P1: TPoint2D;
begin
  P0 := TransformPoint2D(Points[0], ModelTransform);
  P1 := TransformPoint2D(Point2D(Points[1].X, Points[0].Y), ModelTransform);
  fAngle := ArcTan2(P1.Y - P0.Y, P1.X - P0.X);
end;

constructor TEllipse2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2, 50);
  fEllipse2DInsp := TEllipse2DInsp.create(self);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TEllipse2D.destroy;
begin
  fEllipse2DInsp.Free;
  inherited;
end;

constructor TEllipse2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited CreateFromStream(Stream, Version);
  fEllipse2DInsp := TEllipse2DInsp.create(self);
end;

procedure TEllipse2D.GetEllipseParams(var CX, CY, RX, RY: TRealType);
var
  P1, P0: TPoint2D;
begin
  P0:= CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  RX := Abs(P1.X - P0.X) / 2.0 ;
  RY := Abs(P1.Y - P0.Y) / 2.0;
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
end;

function TEllipse2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
  Delta, CurrAngle, CX, RX, CY, RY: TRealType; TmpPointsSet2D: TPointsSet2D;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);

  GetEllipseParams(CX, CY, RX, RY);
  Delta := TWOPI / CurvePrecision;
  ProfilePoints.Add(Point2D(CX + RX, CY));
  CurrAngle := Delta;

  for Cont := 1 to CurvePrecision - 1 do
  begin
    ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
    CurrAngle := CurrAngle + Delta
  end;
  ProfilePoints.Add(Point2D(CX + RX, CY));

  if self.Direction = adCounterClockwise then
  begin
    TmpPointsSet2D := TPointsSet2D.Create(self.ProfilePoints.Count);
    try
      for cont := self.ProfilePoints.Count - 1 downto 0 do
        TmpPointsSet2D.Add(self.ProfilePoints[cont]);
      self.ProfilePoints.Clear;
      self.ProfilePoints.Copy(TmpPointsSet2D, 0, TmpPointsSet2D.Count-1);
    finally
      TmpPointsSet2D.Clear;
      TmpPointsSet2D.Free;
    end;
  end;

  Result := TransformBoundingBox2D(Points.Extension, ModelTransform);
end;

procedure TEllipse2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) or (Obj is TEllipticalArc2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

function TEllipse2D.GetWidth: TRealType;
begin
  result := Abs(Points[1].X - Points[0].X);
end;

procedure TEllipse2D.SetWidth(AValue: TRealType);
var P1: TPoint2D;
begin
  P1 := self.Points[1];
  P1.X := self.Points[0].X + AValue;
  self.Points[1] := P1;
end;

function  TEllipse2D.GetHeight: TRealType;
begin
  result := Abs(Points[1].Y - Points[0].Y);
end;

procedure TEllipse2D.SetHeight(AValue: TRealType);
var P0: TPoint2D;
begin
  P0 := self.Points[0];
  P0.Y := self.Points[1].Y + AValue;
  self.Points[0] := P0;
end;

// =====================================================================
// TFilledEllipse2D
// =====================================================================

procedure TFilledEllipse2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
   if not HasTransform then
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
   else
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
  finally
   EndUseProfilePoints;
  end;
end;

function TFilledEllipse2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                     var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     { Consider all segments in the arc. }
     BeginUseProfilePoints;
     try
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(ProfilePoints.PointsReference, ProfilePoints.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
      EndUseProfilePoints;
     end;
   end;
end;

// =====================================================================
// TBSpline2D
// =====================================================================

function Knot(I, K, N: Integer): Integer;
begin
  if I < K then
   Result := 0
  else if I > N then
   Result := N - K + 2
  else
   Result := I - K + 1;
end;

function NBlend(I, K, OK, N: Integer; U: TRealType): TRealType;
var
  T: Integer;
  V: TRealType;
begin
  if K = 1 then
   begin
     V := 0;
     if (Knot(i, OK, N) <= u) and (u < Knot(i + 1, OK, N)) then
      V := 1;
   end
  else
   begin
     V := 0;
     T := Knot(I + K - 1, OK, N) - Knot(I, OK, N);
     if T <> 0 then
      V := (U - Knot(I, OK, N)) * NBlend(I, K -1, OK, N, U) / T;
     T := Knot(I + K, OK, N) - Knot(I + 1, OK, N);
     if T <> 0 then
      V := V + (Knot(I + K, OK, N) - U) * NBlend(I + 1, K - 1, OK, N, U) / T;
   end;
  Result := V;
end;

function BSpline2D(U: TRealType; N, K: Integer; const Points: TPointsSet2D): TPoint2D;
var
  I: Integer;
  B: TRealType;
  TmpPt: TPoint2D;
begin
  Result := Point2D(0.0, 0.0);
  for I := 0 to N do
   begin
     B := NBlend(I, K, K, N, U);
     TmpPt := CartesianPoint2D(Points[I]);
     Result.X := Result.X + TmpPt.X * B;
     Result.Y := Result.Y + TmpPt.Y * B;
     Result.W := 1.0;
   end;
end;

{function TBSpline2D.PopulateCurvePoints(N: Word): TRect2D;
var Cont: Integer;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  if Points.Count < FOrder then
  begin
    inherited PopulateCurvePoints(Points.Count);
    if FDirection =  adClockwise then
      ProfilePoints.Copy(Points, 0, Points.Count - 1)
    else
      for cont := Points.Count - 1 downto 0  do
        ProfilePoints.Add(Points[cont]);
  end else
  begin
    inherited PopulateCurvePoints(CurvePrecision + 1);
    if FDirection =  adClockwise  then
    begin
      for Cont := 0 to CurvePrecision - 1 do
        ProfilePoints.Add(BSpline2D(Cont / CurvePrecision * (Points.Count - 2),
                                Points.Count - 1, FOrder, Points));
      ProfilePoints.Add(Points[Points.Count - 1]);
    end else
    begin
      ProfilePoints.Add(Points[Points.Count - 1]);
      for Cont := CurvePrecision - 1 downto 0 do
        ProfilePoints.Add(BSpline2D(Cont / CurvePrecision * (Points.Count - 2),
                                Points.Count - 1, FOrder, Points));
    end;
  end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;}

function TBSpline2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  if Points.Count < FOrder then
   begin
     inherited PopulateCurvePoints(Points.Count);
     ProfilePoints.Copy(Points, 0, Points.Count - 1);
   end
  else
   begin
     inherited PopulateCurvePoints(CurvePrecision + 1);
     for Cont := 0 to CurvePrecision - 1 do
      ProfilePoints.Add(BSpline2D(Cont / CurvePrecision * (Points.Count - 2),
                                Points.Count - 1, FOrder, Points));
     ProfilePoints.Add(Points[Points.Count - 1]);
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

constructor TBSpline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 50);
  fOrder := 3; { The default spline is cubic. }
  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
end;

constructor TBSpline2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  with Stream do
   { Load the particular properties. }
   Read(fOrder, SizeOf(fOrder));
end;

procedure TBSpline2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fOrder, SizeOf(fOrder));
end;

procedure TBSpline2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TBSpline2D) then
   begin
     fOrder := TBSpline2D(Obj).fOrder;
     Points.Copy(TPrimitive2D(Obj).Points, 0, TPrimitive2D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
//TText2DInsp///////////////////////////////////////////////////////////////////
function TText2DInsp.GetTextLines: TStringList;
begin
  result := fOwner.TextList;
end;

procedure TText2DInsp.SetTextLines(AStringList: TStringList);
begin
  fOwner.TextList := AStringList;
end;

constructor TText2DInsp.create(AOwner: TText2D);
begin
  fOwner := AOwner;
end;

destructor  TText2DInsp.destroy;
begin
  inherited;
end;

function   TText2DInsp.GetText: TStringList;
begin
  result := fOwner.TextList;
end;

procedure  TText2DInsp.SetText(ATextList: TStringList);
begin
  fOwner.TextList := ATextList;
end;

function   TText2DInsp.GetHeight: TRealType;
begin
  result := fOwner.Height;
end;

procedure  TText2DInsp.SetHeight(AValue: TRealType);
begin
  fOwner.Height := AValue;
end;

function   TText2DInsp.GetColorSource: TColorSource;
begin
  result := fOwner.ColorSource;
end;

procedure  TText2DInsp.SetColorSource(AColorSource: TColorSource);
begin
  fOwner.ColorSource := AColorSource;
end;

function   TText2DInsp.GetColor: TColor;
begin
  result := fOwner.Color;
end;

procedure  TText2DInsp.SetColor(AColor: TColor);
begin
  fOwner.Color := AColor;
end;

function   TText2DInsp.GetClippingFlags: integer;
begin
  result := fOwner.ClippingFlags;
end;

procedure  TText2DInsp.SetClippingFlags(AClippingFlags: integer);
begin
  fOwner.ClippingFlags := AClippingFlags;
end;

function   TText2DInsp.GetAutoSize: boolean;
begin
  result := fOwner.AutoSize;
end;

procedure  TText2DInsp.SetAutoSize(AValue: boolean);
begin
  fOwner.AutoSize := AValue;
end;

function   TText2DInsp.GetDrawBox: boolean;
begin
  result := fOwner.DrawBox;
end;

procedure  TText2DInsp.SetDrawBox(AValue: boolean);
begin
  fOwner.DrawBox := AValue;
end;

function   TText2DInsp.GetFaceName: TFaceName;
begin
  result := fOwner.FaceName;
end;

procedure  TText2DInsp.SetFaceName(AFaceName: TFaceName);
begin
  fOwner.FaceName := AFaceName;
end;

function   TText2DInsp.GetItalic: boolean;
begin
  result := boolean(fOwner.Italic);
end;

procedure  TText2DInsp.SetItalic(AValue: boolean);
begin
  fOwner.Italic := AValue;
end;

function   TText2DInsp.GetStrikeout: boolean;
begin
  result := boolean(fOwner.StrikeOut);
end;

procedure  TText2DInsp.SetStrikeout(AValue: boolean);
begin
  fOwner.StrikeOut := AValue;
end;

function   TText2DInsp.GetUnderline: boolean;
begin
  result := boolean(fOwner.Underline);
end;

procedure  TText2DInsp.SetUnderline(AValue: boolean);
begin
  fOwner.Underline := AValue;
end;

function   TText2DInsp.GetBold: boolean;
begin
  result := fOwner.Bold;
end;

procedure  TText2DInsp.SetBold(AValue: boolean);
begin
  fOwner.Bold := AValue;
end;

// =====================================================================
// TText2D
// =====================================================================

procedure TText2D.InitializeAngle;
begin
  fAngle := 0;
end;

procedure TText2D.SetAngle(AAngle: TRealType);
var relativeAngle: TRealType;  ToPt, DragPt: TPoint2D;
begin
  fAngle := DegToRad(AAngle);
  self.LogFont.Escapement := round(AAngle * 10.0);
  //UpdateExtension(Self);
end;

procedure  TText2D.Reverse; //added
var TmpStr: string; i: integer;
begin
  TmpStr := '';
  for i := Length(self.fText) downto 1 do
    TmpStr := TmpStr + self.fText[i];
  self.fText := TmpStr;
end;

procedure  TText2D.Inverse; //adedd
begin
  self.Reverse;
end;

constructor TText2D.Create(ID: LongInt; Rect1: TRect2D; Height: TRealType;
                           Txt: AnsiString);
begin
  inherited Create(ID, 2);
  fTextList := TStringList.Create;
  fText2DInsp := TText2DInsp.create(self);
  Points.DisableEvents := True;
  try
    fAutoSize := true;
    fHeight := Height;
    fText := Txt;
    fRecalcBox := False;
    fDrawBox := False;
    fExtFont := TExtendedFont.Create;
    WritableBox := Rect1;
    fClippingFlags := $100;
    Points.Add(Rect1.FirstEdge);
    Points.Add(Rect1.SecondEdge);
    Points.GrowingEnabled := False;
    fColor := clSilver;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TText2D.Destroy;
begin
  fTextList.Free;
  fText2DInsp.Free;

  fExtFont.Free;
  inherited Destroy;
end;

function TText2D.GetTextLines: TStringList;
begin
  fTextList.Clear;
  fTextList.Add(fText);
  fTextList.Text := fText;
  result := fTextList;
end;

procedure TText2D.SetTextLines(AStringList: TStringList);
begin
  fText     := AStringList.Text;
  UpdateExtension(nil);
  OwnerCAD.Viewports[0].Repaint;
end;

procedure TText2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpBox: TRect2D;
  TmpHeight: Integer;
  TmpRect: TRect;
  TmpTransf: TTransf2D;
  TmpColor: TColor;
begin
  { Find the correct size. }
  TmpBox.FirstEdge := Point2D(0, 0);
  TmpBox.SecondEdge := Point2D(0, fHeight);
  TmpBox := TransformRect2D(TmpBox, VT);
  TmpHeight := Round(PointDistance2D(TmpBox.FirstEdge, TmpBox.SecondEdge));
  if TmpHeight <= 0 then
   Exit;
  { Build up the DrawText rect. }
  TmpRect := Rect2DToRect(TransformRect2D(Box, VT));
  FExtFont.Canvas := Cnv.Canvas;

  TmpColor := FExtFont.Canvas.Font.Color;

  case fColorSource of
    csCustom: begin
      FExtFont.Canvas.Font.Color := self.fColor;
    end;
    csByLayer: begin
      if self.OwnerCAD <> nil then
        FExtFont.Canvas.Font.Color := self.OwnerCAD.Layers[self.OwnerCAD.CurrentLayer].Pen.Color
      else begin
        FExtFont.Canvas.Font.Color := self.fColor;
        fColorSource := csCustom;
      end;
    end;
    csByBlock: begin
      FExtFont.Canvas.Font.Color := self.fColor;
      fColorSource := csCustom;
    end;
  end;

  try
    FExtFont.Height := TmpHeight;
    if fRecalcBox then
     begin
       LCLIntf.DrawText(Cnv.Canvas.Handle, PChar(FText), Length(FText), TmpRect, DT_CALCRECT);
       if not HasTransform then
        TmpTransf := VT
       else
        TmpTransf := MultiplyTransform2D(ModelTransform, VT);
       TmpBox := TransformBoundingBox2D(RectToRect2D(TmpRect), InvertTransform2D(TmpTransf));
       Points.DisableEvents := True;
       try
         Points[0] := TmpBox.FirstEdge;
         Points[1] := TmpBox.SecondEdge;
       finally
         Points.DisableEvents := False;
         UpdateExtension(Self);
       end;
     end;
    if (Cnv.Canvas.Pen.Mode <> pmXor) then
     LCLIntf.DrawText(Cnv.Canvas.Handle, PChar(FText), Length(FText), TmpRect, FClippingFlags);
    if FDrawBox or (Cnv.Canvas.Pen.Mode = pmXor) then
     DrawRect2DAsPolyline(Cnv, Box, RectToRect2D(Cnv.Canvas.ClipRect), IdentityTransf2D, VT);
  finally
    FExtFont.Canvas := nil;
  end;
end;

function TText2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   Result := PICK_INOBJECT;
end;

constructor TText2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  { Load the standard properties }
  inherited;
  fTextList := TStringList.Create;
  fText2DInsp := TText2DInsp.create(self);
  with Stream do
   begin
     Read(TmpInt, SizeOf(TmpInt));
     SetString(FText, nil, TmpInt);
     Read(Pointer(FText)^, TmpInt);
     FExtFont := TExtendedFont.Create;
     FExtFont.LoadFromStream(Stream);
     Read(FClippingFlags, SizeOf(FClippingFlags));
     Read(FDrawBox, SizeOf(FDrawBox));
     Read(fHeight, SizeOf(fHeight));

     Read(fColorSource, SizeOf(fColorSource));
     Read(fColor, SizeOf(fColor));
     Read(fAutoSize, SizeOf(fAutoSize));
     Read(fItalic, SizeOf(fItalic));
     Read(fStrikeout, SizeOf(fStrikeout));
     Read(fUnderline, SizeOf(fUnderline));
     Read(fBold, SizeOf(fBold));
     Read(fFaceName, SizeOf(fFaceName));
   end;
end;

procedure TText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpInt := Length(FText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(FText)^, Length(FText));
     FExtFont.SaveToStream(Stream);
     Write(FClippingFlags, SizeOf(FClippingFlags));
     Write(FDrawBox, SizeOf(FDrawBox));
     Write(fHeight, SizeOf(fHeight));

     Write(fColorSource, SizeOf(fColorSource));
     Write(fColor, SizeOf(fColor));
     Write(fAutoSize, SizeOf(fAutoSize));
     Write(fItalic, SizeOf(fItalic));
     Write(fStrikeout, SizeOf(fStrikeout));
     Write(fUnderline, SizeOf(fUnderline));
     Write(fBold, SizeOf(fBold));
     Write(fFaceName, SizeOf(fFaceName));
   end;
end;

procedure TText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TText2D) or (Obj is TFrame2D) then
   begin
     if Obj is TText2D then
      begin
        fText := (Obj as TText2D).Text;
        fHeight := (Obj as TText2D).Height;
        fDrawBox := (Obj as TText2D).DrawBox;
        fRecalcBox := (Obj as TText2D).fRecalcBox;
        fClippingFlags := (Obj as TText2D).ClippingFlags;
        if not Assigned(FExtFont) then
         FExtFont := TExtendedFont.Create;
        fExtFont.Assign(TText2D(Obj).fExtFont);
      end;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

function   TText2D.GetFaceName: TFaceName;
begin
  result := fExtFont.FaceName;
end;

procedure  TText2D.SetFaceName(AFaceName: TFaceName);
begin
  fExtFont.FaceName := AFaceName;
end;

function   TText2D.GetItalic: boolean;
begin
  result := boolean(fExtFont.Italic);
end;

procedure  TText2D.SetItalic(AValue: boolean);
begin
  fExtFont.Italic := word(AValue);
end;

function   TText2D.GetStrikeout: boolean;
begin
  result := boolean(fExtFont.StrikeOut);
end;

procedure  TText2D.SetStrikeout(AValue: boolean);
begin
  fExtFont.StrikeOut := word(AValue);
end;

function   TText2D.GetUnderline: boolean;
begin
  result := boolean(fExtFont.Underline);
end;

procedure  TText2D.SetUnderline(AValue: boolean);
begin
  fExtFont.Underline := word(AValue);
end;

function   TText2D.GetBold: boolean;
begin
  result := fExtFont.Weight = 700;
end;

procedure  TText2D.SetBold(AValue: boolean);
begin
  if AValue then
    fExtFont.Weight := 700
  else
    fExtFont.Weight := 400;
end;

////////////////////////////////////////////////////////////////////////////////
//TBitmap2DInsp/////////////////////////////////////////////////////////////////
constructor TBitmap2DInsp.create(AOwner: TBitmap2D);
begin
  fOwner := AOwner;
end;

destructor  TBitmap2DInsp.destroy;
begin
  inherited;
end;

function  TBitmap2DInsp.GetScaleFactor: TRealType;
begin
  result := fOwner.ScaleFactor;
end;

procedure TBitmap2DInsp.SetScaleFactor(SF: TRealType);
begin
  fOwner.ScaleFactor := SF;
end;

function  TBitmap2DInsp.GetAspectRatio: TRealType;
begin
  result := fOwner.AspectRatio;
end;

procedure TBitmap2DInsp.SetAspectRatio(AR: TRealType);
begin
  fOwner.AspectRatio := AR;
end;

function  TBitmap2DInsp.GetCopyMode: TCopyMode;
begin
  result := fOwner.CopyMode;
end;

procedure TBitmap2DInsp.SetCopyMode(ACopyMode: TCopyMode);
begin
  fOwner.CopyMode := ACopyMode;
end;

// =====================================================================
// TBitmap2D
// =====================================================================

procedure TBitmap2D.SetScaleFactor(SF: TRealType);
var
  TmpPt: TPoint2D;
begin
  if( fScaleFactor <> SF ) then
   begin
     fScaleFactor := SF;
     if( fScaleFactor <> 0.0 ) then
      begin
        if( fAspectRatio <> 0.0 ) then
         TmpPt.X := Points[0].X + fBitmap.Height * fScaleFactor / fAspectRatio
        else
         TmpPt.X := Points[0].X + fBitmap.Width * fScaleFactor;
        TmpPt.Y := Points[0].Y + fBitmap.Height * fScaleFactor;
        TmpPt.W := 1.0;
        Points[1] := TmpPt;
      end;
   end;
end;

procedure TBitmap2D.SetAspectRatio(AR: TRealType);
var
  TmpPt: TPoint2D;
begin
  if( fAspectRatio <> AR ) then
   begin
     fAspectRatio := AR;
     if( fScaleFactor <> 0.0 ) then
      begin
        if( fAspectRatio <> 0.0 ) then
         TmpPt.X := Points[0].X + fBitmap.Height * fScaleFactor / fAspectRatio
        else
         TmpPt.X := Points[0].X + fBitmap.Width * fScaleFactor;
        TmpPt.Y := Points[0].Y + fBitmap.Height * fScaleFactor;
        TmpPt.W := 1.0;
        Points[1] := TmpPt;
      end;
   end;
end;

procedure TBitmap2D.InitializeAngle;
begin
  fAngle := 0;
end;


{Bitmap.CopyModes:
cmBlackness:  Draws everything in black.
cmWhiteness:  Draws everything in white.
cmSrcCopy:    Copies the source image unchanged onto the destination surface (default mode).
cmSrcInvert:  Inverts the colors of the source image.
cmMergeCopy:  Combines the source and destination images in a specific way.
cmMergePaint: Another painting method that blends the source and destination images.
}

constructor TBitmap2D.Create(ID: LongInt; const P1, P2: TPoint2D; Bmp: TBitmap);
begin
  inherited Create(ID, 2);
  fBitmap2DInsp := TBitmap2DInsp.create(self);
  fScaleFactor := 0.0;
  fAspectRatio := 0.0;
  //fCopyMode := cmSrcCopy;  //original;
  fCopyMode := cmSrcInvert;
  Points.DisableEvents := True;
  try
    fBitmap := TBitmap.Create;
    fBitmap.Assign(Bmp);
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TBitmap2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited;
  if (Obj is TBitmap2D) or (Obj is TFrame2D) then
  begin
    fScaleFactor := TBitmap2D(Obj).ScaleFactor;
    fAspectRatio := TBitmap2D(Obj).AspectRatio;
    fCopyMode := TBitmap2D(Obj).CopyMode;
    if Obj is TBitmap2D then
    begin
      if fBitmap = nil then  //added
        fBitmap := TBitmap.Create;  //added
      fBitmap.Assign(TBitmap2D(Obj).fBitmap);
    end;
    Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
    Points.GrowingEnabled := True;
  end;
end;

destructor TBitmap2D.Destroy;
begin
  fBitmap2DInsp.Free;

  fBitmap.Free;
  inherited Destroy;
end;

constructor TBitmap2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fBitmap2DInsp := TBitmap2DInsp.create(self);
  fBitmap := TBitmap.Create;
  fBitmap.LoadFromStream(Stream);
  if( Version >= 'CAD422' ) then
   begin
     Stream.Read(fScaleFactor, SizeOf(fScaleFactor));
     Stream.Read(fAspectRatio, SizeOf(fAspectRatio));
     Stream.Read(fCopyMode, SizeOf(fCopyMode));
   end;
end;

procedure TBitmap2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  fBitmap.SaveToStream(Stream);
  Stream.Write(fScaleFactor, SizeOf(fScaleFactor));
  Stream.Write(fAspectRatio, SizeOf(fAspectRatio));
  Stream.Write(fCopyMode, SizeOf(fCopyMode));
end;

procedure TBitmap2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpPt1, TmpPt2: TPoint2D;
  TmpTransf: TTransf2D;
  TmpRect: TRect;
  OldMode: TCopyMode;
begin
  if not HasTransform then
   TmpTransf := VT
  else
   TmpTransf := MultiplyTransform2D(ModelTransform, VT);
  TmpPt1 := TransformPoint2D(Points[0], TmpTransf);
  TmpPt2 := TransformPoint2D(Points[1], TmpTransf);
  TmpRect := Rect2DToRect(Rect2D(TmpPt1.X, TmpPt1.Y, TmpPt2.X, TmpPt2.Y));
  OldMode := Cnv.Canvas.CopyMode;
  Cnv.Canvas.CopyMode := fCopyMode;
  Cnv.Canvas.StretchDraw(TmpRect, fBitmap);
  Cnv.Canvas.CopyMode := OldMode;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectChar.GetVect(Idx: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D(fSubVects[Idx]);
end;

function TVectChar.GetVectCount: Integer;
begin
  Result := fSubVects.NumberOfObjects;
end;

constructor TVectChar.Create(NSubVect: Integer);
var
  Cont: Integer;
begin
  inherited Create;

  fSubVects := TIndexedObjectList.Create(NSubVect);
  fSubVects.FreeOnClear := True;
  for Cont := 0 to NSubVect - 1 do
   fSubVects.Objects[Cont] := TPointsSet2D.Create(0);
end;

destructor TVectChar.Destroy;
begin
  fSubVects.Free;

  inherited;
end;

procedure TVectChar.UpdateExtension;
var
  Cont: Integer;
begin
  fExtension := Rect2D(0.0, 0.0, 0.0, 0.0);
  for Cont := 0 to fSubVects.NumberOfObjects - 1 do
   fExtension := BoxOutBox2D(fExtension, TPointsSet2D(fSubVects.Objects[Cont]).Extension);
end;

constructor TVectChar.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
begin
  inherited Create;
  if( Version >= 'CAD4  ' ) then
   with Stream do
    begin
      Read(TmpInt, SizeOf(TmpInt));
      fSubVects := TIndexedObjectList.Create(TmpInt);
      // Lettura vettori.
      for Cont := 0 to fSubVects.NumberOfObjects - 1 do
       begin
         Read(TmpWord, SizeOf(TmpWord));
         fSubVects.Objects[Cont] := TPointsSet2D.Create(TmpWord);
         while TmpWord > 0 do
          begin
            Read(TmpPt, SizeOf(TmpPt));
            TPointsSet2D(fSubVects.Objects[Cont]).Add(TmpPt);
            Dec(TmpWord);
          end;
       end;
      UpdateExtension(Self);
    end;
end;

procedure TVectChar.SaveToStream(const Stream: TStream);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
begin
  with Stream do
   begin
     TmpInt := fSubVects.NumberOfObjects;
     Write(TmpInt, SizeOf(TmpInt));
     // Scrittura vettori.
     for Cont := 0 to fSubVects.NumberOfObjects - 1 do
      begin
        TmpWord := TPointsSet2D(fSubVects.Objects[Cont]).Count;
        Write(TmpWord, SizeOf(TmpWord));
        while TmpWord > 0 do
         begin
           TmpPt := TPointsSet2D(fSubVects.Objects[Cont]).Points[TmpWord - 1];
           Write(TmpPt, SizeOf(TmpPt));
           Dec(TmpWord);
         end;
      end;
   end;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectFont.GetChar(Ch: Char): TVectChar;
begin
  Result := TVectChar(fVects[Ord(Ch)]);
end;

constructor TVectFont.Create;
begin
  inherited;

  // Al massimo ci sono 256 caratteri.
  fVects := TIndexedObjectList.Create(256);
  fVects.FreeOnClear := True;
end;

destructor TVectFont.Destroy;
begin
  fVects.Free;

  inherited;
end;

constructor TVectFont.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  inherited Create;
  with Stream do
   begin
     fVects := TIndexedObjectList.Create(256);
     // Lettura caratteri.
     while True do
      begin
        // Lettura numero carattere.
        Read(TmpInt, SizeOf(TmpInt));
        if TmpInt = -1 then
         Break; // Fine.
        fVects[TmpInt] := TVectChar.CreateFromStream(Stream, Version);
      end;
   end;
end;

procedure TVectFont.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  with Stream do
   begin
     // Scrittura caratteri.
     for TmpInt := 0 to fVects.NumberOfObjects - 1 do
      if fVects[TmpInt] <> nil then
       begin
         // Scrittura numero carattere.
         Write(TmpInt, SizeOf(TmpInt));
         TVectChar(fVects[TmpInt]).SaveToStream(Stream);
       end;
     // Fine
     TmpInt := -1;
     Write(TmpInt, SizeOf(TmpInt));
   end;
end;

function TVectFont.CreateChar(Ch: Char; N: Integer): TVectChar;
begin
  if fVects[Ord(Ch)] <> nil then
   fVects[Ord(Ch)].Free;
  Result := TVectChar.Create(N);
  fVects[Ord(Ch)] := Result;
end;

procedure TVectFont.DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv: TDecorativeCanvas);
var
  Cont: Integer;
  TmpTransf: TTransf2D;
  TmpExt: TRect2D;
  TmpCh: TVectChar;
begin
  TmpCh := nil;
  if (Ch = ' ') then
   begin
     DrawPoint.X := DrawPoint.X + (0.4 + ICS) * H;
     Exit;
   end
  else if fVects[Ord(Ch)] <> nil then
   TmpCh := GetChar(Ch)
  else if (Ch <> #13) then
   TmpCh := _NullChar;
  if TmpCh <> nil then
   begin
     TmpTransf := IdentityTransf2D;
     TmpTransf[1, 1] := H;
     TmpTransf[2, 2] := H;
     TmpTransf[3, 1] := DrawPoint.X;
     TmpTransf[3, 2] := DrawPoint.Y;
     TmpTransf[3, 3] := DrawPoint.W;
     with TmpCh do
      begin
        TmpExt := Extension;
        for Cont := 0 to VectorCount - 1 do
         Vectors[Cont].DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), TmpExt, MultiplyTransform2D(TmpTransf, VT));
        DrawPoint.X := DrawPoint.X + (TmpExt.Right + ICS) * H;
      end;
   end;
end;

function TVectFont.GetTextExtension(Str: AnsiString; H, InterChar, InterLine: TRealType): TRect2D;
var
  Cont: Integer;
  RowLen, RowHeight, MaxRowLen: TRealType;
begin
  // Per i caratteri con la gambetta (come g) parto con Bottom = 1.0
  Result := Rect2D(0.0, 1.0, 0.0, H);
  MaxRowLen := 0.0;
  RowLen := 0.0;
  RowHeight := 0.0;
  for Cont := 1 to Length(Str) do
   begin
     if fVects[Ord(Str[Cont])] <> nil then
      with GetChar(Str[Cont]).Extension do
       begin
         RowLen := RowLen + (Right + InterChar);
         // Bottom contiene l'eventuale gambetta. 
         RowHeight := MaxValue([RowHeight, 1.0 - Bottom]);
       end
     else if Str[Cont] = ' ' then
      // Space.
      RowLen := RowLen + (0.5 + InterChar)
     else if Str[Cont] <> #13 then
      // Carattere nullo.
      with _NullChar.Extension do
       RowLen := RowLen + (Right + InterChar);
     if Str[Cont] = #13 then
      begin
        // New line. L'altezza � 1.3 per via delle gambette.
        MaxRowLen := MaxValue([MaxRowLen, RowLen - InterChar]);
        Result.Bottom := Result.Bottom - (InterLine + RowHeight);
        RowLen := 0.0;
        RowHeight := 0.0;
      end;
   end;
  MaxRowLen := MaxValue([MaxRowLen, RowLen - InterChar]);
  Result.Left := 0.0;
  Result.Bottom := (Result.Bottom - RowHeight) * H;
  Result.Right := MaxRowLen * H;
end;

// =====================================================================
// Registration functions
// =====================================================================

function CADSysFindFontIndex(const Font: TVectFont): Word;
var
  Cont: Integer;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   if Assigned(VectFonts2DRegistered[Cont]) and
      (VectFonts2DRegistered[Cont] = Font) then
    begin
      Result := Cont;
      Exit;
    end;
  Raise ECADObjClassNotFound.Create('CADSysFindFontIndex: Font not found');
end;

function CADSysFindFontByIndex(Index: Word): TVectFont;
begin
  if not Assigned(VectFonts2DRegistered[Index]) then
   begin
     if Assigned(_DefaultFont) then
      Result := _DefaultFont
     else
      Raise ECADObjClassNotFound.Create('CADSysFindFontByIndex: Font not registered');
   end
  else
   Result := VectFonts2DRegistered[Index];
end;

procedure CADSysRegisterFont(Index: Word; const Font: TVectFont);
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysRegisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFont: Font index already allocated');
  VectFonts2DRegistered[Index] := Font;
end;

procedure CADSysUnregisterFont(Index: Word);
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysUnregisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   begin
     VectFonts2DRegistered[Index].Free;
     VectFonts2DRegistered[Index] := nil;
   end;
end;

procedure CADSysRegisterFontFromFile(Index: Word; const FileName: String);
var
  TmpStream: TFileStream;
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysRegisterFontFromFile: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFontFromFile: Font index already allocated');
  TmpStream := TFileStream.Create(FileName, fmOpenRead);
  try
   VectFonts2DRegistered[Index] := TVectFont.CreateFromStream(TmpStream, CADSysVersion);
  finally
   TmpStream.Free;
  end;
end;

procedure CADSysInitFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   VectFonts2DRegistered[Cont] := nil;
end;

procedure CADSysClearFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   if Assigned(VectFonts2DRegistered[Cont]) then
    VectFonts2DRegistered[Cont].Free;
end;

function CADSysGetDefaultFont: TVectFont;
begin
  Result := _DefaultFont;
end;

procedure CADSysSetDefaultFont(const Font: TVectFont);
begin
  _DefaultFont := Font;
end;

// =====================================================================
// TJustifiedVectText2DInsp
// =====================================================================
constructor TJustifiedVectText2DInsp.create(AOwner: TJustifiedVectText2D);
begin
  inherited create;
  fOwner :=  AOwner;
  fBasePoint := TCoordPoint2DInsp.create(fOwner, POINT_CODE_BASE_POINT);
end;

destructor  TJustifiedVectText2DInsp.destroy;
begin
  fBasePoint.Free;
  inherited;
end;

function  TJustifiedVectText2DInsp.GetCharSpace: TRealType;
begin
  result := fOwner.CharSpace;
end;

procedure TJustifiedVectText2DInsp.SetCharSpace(AValue: TRealType);
begin
  fOwner.CharSpace := AValue;
end;

function  TJustifiedVectText2DInsp.GetDrawBox: boolean;
begin
  result := fOwner.DrawBox;
end;

procedure TJustifiedVectText2DInsp.SetDrawBox(AValue: boolean);
begin
  fOwner.DrawBox := AValue;
end;

function  TJustifiedVectText2DInsp.GetHeight: TRealType;
begin
  result := fOwner.Height;
end;

procedure TJustifiedVectText2DInsp.SetHeight(AValue: TRealType);
begin
  fOwner.Height := AValue;
end;

function  TJustifiedVectText2DInsp.GetHJustification: THJustification;
begin
  result := fOwner.HorizontalJust;
end;

procedure TJustifiedVectText2DInsp.SetHJustification(AValue: THJustification);
begin
  fOwner.HorizontalJust := AValue;
end;

function  TJustifiedVectText2DInsp.GetVJustification: TVJustification;
begin
  result := fOwner.VerticalJust;
end;

procedure TJustifiedVectText2DInsp.SetVJustification(AValue: TVJustification);
begin
  fOwner.VerticalJust := AValue;
end;

function  TJustifiedVectText2DInsp.GetInterline: TRealType;
begin
  result := fOwner.InterLine;
end;

procedure TJustifiedVectText2DInsp.SetInterline(AValue: TRealType);
begin
  fOwner.InterLine := AValue;
end;

function  TJustifiedVectText2DInsp.GetText: string;
begin
  result := fOwner.Text;
end;

procedure TJustifiedVectText2DInsp.SetText(AValue: string);
begin
  fOwner.Text := AValue;
end;

function  TJustifiedVectText2DInsp.GetFontIndex: word;
begin
  result := fOwner.FontIndex;
end;

procedure TJustifiedVectText2DInsp.SetFontIndex(AValue: word);
begin
  fOwner.FontIndex := AValue;
end;

// =====================================================================
// TJustifiedVectText2D
// =====================================================================

procedure TJustifiedVectText2D.SetHeight(H: TRealType);
begin
  fHeight := H;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetCharSpace(S: TRealType);
begin
  fCharSpace := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetInterLine(S: TRealType);
begin
  fInterLine := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetText(T: String);
begin
  fText := T;
  UpdateExtension(Self);
end;

function TJustifiedVectText2D.GetTextExtension: TRect2D;
var
  TmpRect: TRect2D;
  CurrRect: TRect2D;
  DX, TX, TY: TRealType;
begin
  if Assigned(fVectFont) then
   TmpRect := fVectFont.GetTextExtension(fText, fHeight, fCharSpace, fInterLine)
  else
   TmpRect := Rect2D(0, 0, 0, 0);
  CurrRect.FirstEdge := Points[0];
  CurrRect.SecondEdge := Points[1];
  CurrRect := ReorderRect2D(CurrRect);
  fBasePoint := Point2D(CurrRect.Left, CurrRect.Top - TmpRect.Top);
  DX := TmpRect.Right - TmpRect.Left;
  case fHJustification of
   jhLeft: TX := CurrRect.Left;
   jhRight: TX := CurrRect.Right - DX;
   jhCenter: TX := (CurrRect.Left + CurrRect.Right - DX) / 2.0;
  else
   TX := CurrRect.Left;
  end;
  case fVJustification of
   jvTop: TY := CurrRect.Top - TmpRect.Top;
   jvBottom: TY := CurrRect.Bottom - TmpRect.Bottom;
   jvCenter: TY := (CurrRect.Top + CurrRect.Bottom) / 2.0;
  else
   TY := CurrRect.Top - TmpRect.Top;
  end;
  Result := TransformRect2D(TmpRect, Translate2D(TX, TY));
end;

procedure TJustifiedVectText2D.DrawText(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  procedure DrawTextLine(BasePt: TPoint2D; Str: String; ObjTransf: TTransf2D);
  var
    Cont: Integer;
  begin
    for Cont := 1 to Length(Str) do
     // Disegno il carattere.
     fVectFont.DrawChar2D(Str[Cont], BasePt, fHeight, fCharSpace, ObjTransf, Cnv);
  end;
var
  TmpTransf: TTransf2D;
  TmpStr, TmpRow: String;
  CurrBasePt: TPoint2D;
  TmpPos: Integer;
  TmpTExt: TRect2D;
begin
  if not Assigned(fVectFont) then
   Exit;
  // sposto il testo, applico la trasformazione oggetto al testo e trasformo nel viewport.
  TmpTransf := MultiplyTransform2D(ModelTransform, VT);
  try
    TmpTExt := GetTextExtension;
    if fDrawBox then
     DrawRect2DAsPolyline(Cnv, TmpTExt, RectToRect2D(Cnv.Canvas.ClipRect), ModelTransform, VT);
    if DrawMode and DRAWMODE_VECTTEXTONLYBOX = DRAWMODE_VECTTEXTONLYBOX then
     Exit;
    CurrBasePt.X := TmpTExt.Left;
    CurrBasePt.Y := TmpTExt.Top - fHeight;
    CurrBasePt.W := 1.0;
    // Estraggo le righe.
    TmpStr := fText;
    TmpPos := Pos(#13, TmpStr);
    while TmpPos > 0 do
     begin
       TmpRow := Copy(TmpStr, 1, TmpPos - 1);
       Delete(TmpStr, 1, TmpPos);
       if TmpStr[1] = #10 then
        Delete(TmpStr, 1, 1);
       TmpPos := Pos(#13, TmpStr);
       // Draw the string.
       TmpTExt := fVectFont.GetTextExtension(TmpRow, fHeight, fCharSpace, fInterLine);
       DrawTextLine(CurrBasePt, TmpRow, TmpTransf);
       CurrBasePt.Y := CurrBasePt.Y - (TmpTExt.Top - TmpTExt.Bottom) - fHeight * fInterLine;
     end;
    // Draw the string.
    DrawTextLine(CurrBasePt, TmpStr, TmpTransf);
  finally
  end;
end;


//added
function  TJustifiedVectText2D.GetFontIndex: word;
begin
  result := CADSysFindFontIndex(self.fVectFont)
end;

procedure TJustifiedVectText2D.SetFontIndex(AValue: word);
begin
  self.fVectFont := CADSysFindFontByIndex(AValue);
end;

procedure TJustifiedVectText2D.InitializeAngle;
begin
  fAngle := 0;
end;

constructor TJustifiedVectText2D.Create(ID: LongInt; FontVect: TVectFont; TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
begin
  inherited Create(ID, 2);
  fJustifiedVectText2DInsp := TJustifiedVectText2DInsp.create(self);
  Points.DisableEvents := True;
  try
    Points.Add(TextBox.FirstEdge);
    Points.Add(TextBox.SecondEdge);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
  end;

  fHeight := Height;
  fCharSpace := 0.1;
  fInterLine := 0.02;
  fText := Txt;
  fDrawBox := False;
  fVectFont := FontVect;
  fHJustification := jhLeft;
  fVJustification := jvTop;

  UpdateExtension(Self);
end;

destructor TJustifiedVectText2D.destroy;
begin
  fJustifiedVectText2DInsp.Free;
  inherited;
end;

constructor TJustifiedVectText2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  { Load the standard properties }
  inherited;
  fJustifiedVectText2DInsp := TJustifiedVectText2DInsp.create(self);
  with Stream do
   begin
     Read(TmpInt, SizeOf(TmpInt));
     SetString(fText, nil, TmpInt);
     Read(Pointer(fText)^, TmpInt);
     // Lettura indice font.
     Read(TmpInt, SizeOf(TmpInt));
     try
      fVectFont := CADSysFindFontByIndex(TmpInt);
     except
      on ECADObjClassNotFound do
       begin
         ShowMessage('Font class not found. Font not assigned');
         fVectFont := nil;
       end;
     end;
     Read(fHJustification, SizeOf(fHJustification));
     Read(fVJustification, SizeOf(fVJustification));
     Read(fDrawBox, SizeOf(fDrawBox));
     Read(fHeight, SizeOf(fHeight));
     Read(fInterLine, SizeOf(fInterLine));
     Read(fCharSpace, SizeOf(fCharSpace));
   end;
end;

procedure TJustifiedVectText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TJustifiedVectText2D) or (Obj is TText2D) then
   begin
     if not Assigned(Points) then
      begin
        Points := CreateVect(2);
        Points.GrowingEnabled := False;
        Points.OnChange := UpdateExtension;
      end;
     if Obj is TJustifiedVectText2D then
      begin
        fText := (Obj as TJustifiedVectText2D).Text;
        fHeight := (Obj as TJustifiedVectText2D).Height;
        fInterLine := (Obj as TJustifiedVectText2D).InterLine;
        fCharSpace := (Obj as TJustifiedVectText2D).CharSpace;
        fDrawBox := (Obj as TJustifiedVectText2D).DrawBox;
        fVectFont := (Obj as TJustifiedVectText2D).fVectFont;
        fHJustification := (Obj as TJustifiedVectText2D).fHJustification;
        fVJustification := (Obj as TJustifiedVectText2D).fVJustification;
      end
     else if Obj is TText2D then
      begin
        fText := (Obj as TText2D).Text;
        fHeight := (Obj as TText2D).Height;
        fDrawBox := (Obj as TText2D).DrawBox;
      end;
     Points.Clear;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
   end;
end;

procedure TJustifiedVectText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := Length(fText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(fText)^, TmpInt);
     // Scrittura indice font.
     TmpInt := CADSysFindFontIndex(fVectFont);
     Write(TmpInt, SizeOf(TmpInt));
     Write(fHJustification, SizeOf(fHJustification));
     Write(fVJustification, SizeOf(fVJustification));
     Write(fDrawBox, SizeOf(fDrawBox));
     Write(fHeight, SizeOf(fHeight));
     Write(fInterLine, SizeOf(fInterLine));
     Write(fCharSpace, SizeOf(fCharSpace));
   end;
end;

procedure TJustifiedVectText2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  DrawText(VT, Cnv, DrawMode);
end;

function TJustifiedVectText2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  TmpVect: TPointsSet2D;
  TmpDist: TRealType;
  TmpBox: TRect2D;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) then
   begin
     TmpBox := GetTextExtension;
     TmpVect := TPointsSet2D.Create(4);
     try
       TmpVect.Add(TmpBox.FirstEdge);
       TmpVect.Add(Point2D(TmpBox.Left, TmpBox.Top));
       TmpVect.Add(TmpBox.SecondEdge);
       TmpVect.Add(Point2D(TmpBox.Right, TmpBox.Bottom));
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(TmpVect.PointsReference, TmpVect.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
       TmpVect.Free;
     end;
   end;
end;


procedure TJustifiedVectText2D._UpdateExtension;
begin
  inherited;
  WritableBox := TransformBoundingBox2D(GetTextExtension, ModelTransform);
end;

initialization
  {CADSysRegisterClass(3, TLine2D);
  CADSysRegisterClass(4, TPolyline2D);
  CADSysRegisterClass(5, TPolygon2D);
  CADSysRegisterClass(6, TRectangle2D);

  CADSysRegisterClass(7, TEllipticalArc2D);
  CADSysRegisterClass(8, TEllipse2D);
  CADSysRegisterClass(9, TFilledEllipse2D);
  CADSysRegisterClass(10, TText2D);
  CADSysRegisterClass(11, TFrame2D);
  CADSysRegisterClass(12, TBitmap2D);
  CADSysRegisterClass(13, TBSpline2D);
  CADSysRegisterClass(14, TJustifiedVectText2D);

  CADSysRegisterClass(15, TSegment2D);
  CADSysRegisterClass(16, TSector2D);

  CADSysRegisterClass(17, TSymetricSymbol2D);
  CADSysRegisterClass(18, TASymetricSymbol2D);

  CADSysRegisterClass(19, TSimplePrimitive2D);
  CADSysRegisterClass(20, TDirectionalCurve2D);
  CADSysRegisterClass(21, TClosedCurve2D);
  CADSysRegisterClass(22, TClosedPolyline2D);

  CADSysRegisterClass(23, TCircle2D);
  }
  _DefaultHandler2D := TPrimitive2DHandler.Create(nil);

  // Vectorial fonts
  CADSysInitFontList;

  _NullChar := TVectChar.Create(1);
  _NullChar.Vectors[0].Add(Point2D(0.0, 0.0));
  _NullChar.Vectors[0].Add(Point2D(0.8, 0.0));
  _NullChar.UpdateExtension(nil);
  _DefaultFont := nil;



finalization
  CADSysClearFontList;
  _NullChar.Free;
  _DefaultHandler2D.Free;

end.

