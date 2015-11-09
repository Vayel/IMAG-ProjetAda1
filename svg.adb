with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package body SVG is
  contents: Unbounded_String := To_Unbounded_String("");

  procedure init is
  begin
    contents := "";
  end;

  procedure Header(w, h: Integer) is
  begin
    contents := contents & "<svg width=""" & Integer'image(w) & """"; 
    contents := contents & " height=""" & Integer'image(h) & """>";
  end;

  procedure Footer is
  begin
    contents := contents & "</svg>";
  end;

  procedure Polygon(pts: Points; width, color: String) is
  begin
    contents :=  contents & "<polygon points=""";

    for k in  pts'range loop
      contents := contents & Integer'image(pts(k).x) & ",";
      contents := contents & Integer'image(pts(k).y) & " ";
    end loop;

    contents := contents & """ style=""stroke-width:" & width & ";";
    contents := contents &  "stroke:" & color & """ />";
  end;

  function get_contents return String is
  begin
    return to_string(contents);
  end;
end SVG;
