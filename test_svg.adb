with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SVG; use SVG;


procedure testSvg is
  s : Unbounded_String;
  pts : Points(0..2);
begin
  init;
  header(500, 250);
  
  pts(0) := Point(x => 0.0, y => 0.0);
  pts(1) := Point(x => 5.0, y => 0.0);
  pts(2) := Point(x => 3.0, y => 3.0);
  polygon(pts, 0.1, "FF0000");

  footer;

  s := get_contents;
  put(to_string(s));
end;
