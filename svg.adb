package body SVG is
	function Header(w, h: Integer) return String is
  begin
    return "<svg width=""" & Integer'image(w) & """ height=""" & Integer'image(h) & """>";
  end;

	function Footer return String is
  begin
    return "</svg>";
  end;

	function Polygon(pts: Points, width, color: String) return String is
  begin
    null; -- TODO
  end;
end SVG;
