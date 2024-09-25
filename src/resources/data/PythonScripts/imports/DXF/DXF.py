import ezdxf
import CADSys4

OpenFileName = dxffilename.value
DEG_TO_RAD = 0.01745329251994329577

def AddLayers(doc):
  for layer in doc.layers:
    if layer != '0':
      print(layer.dxf.name)
      AName       = layer.dxf.name
      AActive     = not layer.is_locked()
      AVisible    = layer.is_on()
      APenColor   = layer.dxf.color
      APenWidth   = 1  #layer.dxf.thickness
      APenStyle   = layer.dxf.linetype  #'psSolid' ???
      ABrushColor = 5  #layer.dxf.brush_color
      ABrushStyle = 'bssolid'  #layer.dxf.brushtype

      CADSys4.CAD_NewLayer(AName, AActive, AVisible, APenColor, APenWidth, APenStyle, ABrushColor, ABrushStyle);

################################################################################
#def draw_point_entity(e):
#  #Litecad64.lcBlockAddPoint(hLcBlock, e.dxf.location.x, e.dxf.location.y)
#  pass
################################################################################
def draw_line_entity(e):
 PenWidth = 1
 #PenStyle = 0
 CADSys4.CAD_AddLine(e.dxf.start.x, e.dxf.start.y ,e.dxf.end.x , e.dxf.end.y,  e.dxf.color, PenWidth, e.dxf.linetype, e.dxf.layer)
################################################################################
def draw_arc_entity(e):
  cp = e.dxf.center
  sa = e.dxf.start_angle
  ea = e.dxf.end_angle
  r  = e.dxf.radius
  cx = cp[0]
  cy = cp[1]
  layer    = e.dxf.layer
  color    = e.dxf.color
  linetype = e.dxf.linetype
  linewidth = e.dxf.thickness

  CADSys4.CAD_AddArc(cx, cy, r, sa, ea, 0, 0, color, linewidth, linetype, layer)
################################################################################
def draw_circle_entity(e):
  cp = e.dxf.center
  r  = e.dxf.radius
  cx = cp[0]
  cy = cp[1]
  sa = 0.0
  layer    = e.dxf.layer
  color    = e.dxf.color
  linetype = e.dxf.linetype
  linewidth = e.dxf.thickness

  CADSys4.CAD_AddCircle(cx, cy, r, sa, 0, 0, color, linewidth, linetype, layer)
################################################################################
def read_dxf_file():
  doc = ezdxf.readfile(OpenFileName.decode("utf-8"))
  msp = doc.modelspace()
  AddLayers(doc)
  for e in msp:
    if e.dxftype() == "LINE":
      draw_line_entity(e)
    elif e.dxftype() == "ARC":
      draw_arc_entity(e)
    elif e.dxftype() == "CIRCLE":
      draw_circle_entity(e)
  CADSys4.CAD_Repaint(1)
################################################################################
def main():
  read_dxf_file()

if __name__ == "__main__":
  main()


