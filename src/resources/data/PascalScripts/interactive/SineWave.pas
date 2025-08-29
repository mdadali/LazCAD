procedure SineWave;
var
  i: Integer;
  TmpID: Integer;
  color: TColor;
  P0, P1: TPoint2D;
begin
  for i := 0 to 400 do
  begin
    P0.X := i;
    P0.Y := 10 * Sin(P0.X * 0.07);
    P0.W := 1;
    P1.X := P0.X + 100;
    P1.Y := P0.Y + 100;
    P1.W := 1;

    TmpID := CAD_DrawLine2D(-1, P0, P1);

    case RandomRange(0, 6) of
      0: color := clRed;
      1: color := clGreen;
      2: color := clBlue;
      3: color := clYellow;
      //4: color := clMagenta;
      //5: color := clCyan;
      else color := clWhite;
    end;

    CAD_Prim2DSetPenSource(TmpID, psCustom);
    CAD_Prim2DSetPenColor(TmpID, color);

    CAD_ZoomToExtentions;
  end;
end;

begin
  Self.Hide;
  CAD_Clear;
  SineWave;
  Self.Show;
end.

