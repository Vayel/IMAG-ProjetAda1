with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SVG; use SVG;


procedure testSvg is
  pts : Points(0..2);
begin
  init;
  header(10, 10);
  
  pts(0) := (x => 0.0, y => 5.0);
  pts(1) := (x => 2.0, y => 5.0);
  pts(2) := (x => 2.0, y => 3.0);
  polygon(pts, 0.1, "FF0000");

  footer;

  put(get_contents);
end;
