with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package body SVG is
  contents: Unbounded_String;

  procedure init is
  begin
    contents := to_unbounded_string("");
  end;

  procedure header(w, h: Integer) is
  begin
    contents := contents & "<svg width=""" & Integer'image(w) & """"; 
    contents := contents & " height=""" & Integer'image(h) & """>";
  end;

  procedure footer is
  begin
    contents := contents & "</svg>";
  end;

  procedure polygon(pts: Points; width: Float; color: String) is
  begin
    contents :=  contents & "<polygon points=""";

    for k in  pts'range loop
      contents := contents & Float'image(pts(k).x) & ",";
      contents := contents & Float'image(pts(k).y) & " ";
    end loop;

    contents := contents & """ style=""stroke-width:" & Float'image(width) & ";";
    contents := contents & "stroke:#" & color & """ />";
  end;

  function get_contents return String is
  begin
    return to_string(contents);
  end;
end SVG;
