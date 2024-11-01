unit CADDocument;

//{$mode DELPHI}{$H+}
{$mode objfpc}{$H+}

interface


uses ComCtrls,
     CADSys4,
     CS4BaseTypes,
     CS4Shapes,

     RTTIGrids,
     PropEdits,
     ObjectInspector;

var
    CADPrg2D:  TCADPrg2D;
    CADCmp2D:  TCADCmp2D;
    CADViewport2D: TCADViewport2D;
    ProgressBar: TProgressBar;
    PropertyGrid: TTIPropertyGrid;


procedure RegisterDocument(ACADPrg2D: TCADPrg2D; ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D;
                          AProgressBar: TProgressBar; APropertyGrid: TTIPropertyGrid);

procedure GetDocument(var ACADPrg2D: TCADPrg2D; var ACADCmp2D: TCADCmp2D; var ACADViewport2D: TCADViewport2D;
                          var AProgressBar: TProgressBar; var APropertyGrid: TTIPropertyGrid);


implementation

procedure RegisterDocument(ACADPrg2D: TCADPrg2D; ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D;
                          AProgressBar: TProgressBar; APropertyGrid: TTIPropertyGrid);
begin
  CADPrg2D      := ACADPrg2D;
  CADCmp2D      := ACADCmp2D;
  CADViewport2D := ACADViewport2D;
  ProgressBar   := AProgressBar;
  PropertyGrid  := APropertyGrid;
end;

procedure GetDocument(var ACADPrg2D: TCADPrg2D; var ACADCmp2D: TCADCmp2D; var ACADViewport2D: TCADViewport2D;
                          var AProgressBar: TProgressBar; var APropertyGrid: TTIPropertyGrid);
begin
  ACADPrg2D      := CADPrg2D;
  ACADCmp2D      := CADCmp2D;
  ACADViewport2D := CADViewport2D;
  AProgressBar   := ProgressBar;
  APropertyGrid  := PropertyGrid;
end;

initialization
  CADPrg2D      := nil;
  CADCmp2D      := nil;
  CADViewport2D := nil;
  ProgressBar   := nil;
  PropertyGrid  := nil;
finalization

end.
