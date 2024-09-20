from PySide6.QtWidgets import QApplication, QWidget, QFileDialog, QVBoxLayout, QPushButton
import os
import sys
import Litecad64
import ezdxf

#Delphi variables
hLcBlock = hLcBlockVar.Value
hLcDrw   = hLcDrwVar.Value
hLcWnd   = hLcWndVar.Value
OpenFileName = OpenFileNameVar.Value

DEG_TO_RAD = Litecad64.LC_DEG_TO_RAD



################################################################################
def draw_point_entity(e):
  Litecad64.lcBlockAddPoint(hLcBlock, e.dxf.location.x, e.dxf.location.y)

################################################################################
def draw_line_entity(e):
  Litecad64.lcBlockAddLine(hLcBlock, e.dxf.start.x, e.dxf.start.y ,e.dxf.end.x ,e.dxf.end.y)

################################################################################
def draw_arc_entity(e):
  start_angle = (DEG_TO_RAD * e.dxf.start_angle)
  end_angle   = (DEG_TO_RAD * e.dxf.end_angle)
  radius      =  e.dxf.radius

  arc_angle = end_angle - start_angle
  if (arc_angle < 0):
    arc_angle = (arc_angle + (2 * 3.14))
  else:
    if (arc_angle > (2 * 3.14)):
      arc_angle = (arc_angle - (2 * 3.14))

  hEnt =Litecad64.lcBlockAddArc(hLcBlock, e.dxf.center.x, e.dxf.center.y , e.dxf.radius, start_angle, arc_angle)
  if arc_angle >= 0:
    Litecad64.lcPropPutInt(hEnt, Litecad64.LC_PROP_ARC_CCW, 1)
  else:
    Litecad64.lcPropPutInt(hEnt, Litecad64.LC_PROP_ARC_CCW, 0)

################################################################################
def draw_circle_entity(e):
  hEnt =Litecad64.lcBlockAddCircle(hLcBlock, e.dxf.center.x, e.dxf.center.y , e.dxf.radius, 0)

################################################################################
def read_dxf_file():
  doc = ezdxf.readfile(OpenFileName.decode("utf-8"))
  msp = doc.modelspace()
  Litecad64.lcBlockClear(hLcBlock, 0)
  # iterate over all entities in modelspace
  for e in msp:
    if e.dxftype() == "POINT":
      draw_point_entity(e)
    elif e.dxftype() == "LINE":
      draw_line_entity(e)
    elif e.dxftype() == "ARC":
      draw_arc_entity(e)
    elif e.dxftype() == "CIRCLE":
      draw_circle_entity(e)

  Litecad64.lcWndRedraw(hLcWnd)
  Litecad64.lcWndExeCommand( hLcWnd, Litecad64.LC_CMD_ZOOM_EXT, 0 );

################################################################################
def main():
  read_dxf_file()

if __name__ == "__main__":
  main()


