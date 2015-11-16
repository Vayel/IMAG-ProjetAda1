with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SVG; use SVG;


procedure testSvg is
begin
  init;
  header(10, 10);
  
  startPolygon(0.1, "FF0000");
  addPolyPoint(x => 1.0, y => 1.0);
  addRelPolyPoint(dx => 1.0, dy => 3.0);
  addPolyPoint(x => 2.0, y => 2.0);
  endPolygon;

  footer;

  put(get_contents);
end;
