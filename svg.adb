with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;


package body SVG is
  contents: Unbounded_String;
  newLine: String(1..1) := (1 => Ada.Characters.Latin_1.LF);
  curX, curY: Float := 0.0;

  procedure init is
  begin
    contents := to_unbounded_string("");
  end;

  procedure addLine is
  begin
    contents := contents & newLine;
  end;

  procedure header(w, h: Integer) is
  begin
    contents := contents & "<svg width=""" & Integer'image(w) & """"; 
    contents := contents & " height=""" & Integer'image(h) & """>";
    addLine;
    addLine;
  end;

  procedure footer is
  begin
    contents := contents & "</svg>";
  end;

  procedure startPolygon(lineWidth: Float; color: String) is
  begin
    contents := contents & "<polygon";
    contents := contents & " style=""fill:none;stroke-width:" & Float'image(lineWidth) & ";";
    contents := contents & "stroke:#" & color & """";
    contents := contents & " points=""";
  end;

  procedure move(x, y: Float) is
  begin
    curX := x;
    curY := y;
  end;

  procedure addPolyPoint(x, y: Float) is
  begin
    contents := contents & Float'image(x) & ",";
    contents := contents & Float'image(y) & " ";
    
    move(x, y);
  end;

  procedure addRelPolyPoint(dx, dy: Float) is
  begin
    addPolyPoint(curX + dx, curY + dy);
  end;

  procedure endPolygon is
  begin
    contents := contents & """ />";
    addLine;
    addLine;
  end;

  function get_contents return String is
  begin
    return to_string(contents);
  end;
end SVG;
