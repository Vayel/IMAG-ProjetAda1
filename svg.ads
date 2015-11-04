-- Ce module facilite la création de fichier SVG.

package SVG is
  type Point is record
    x: Natural;
    y: Natural;
  end record;

  type Points is array (Integer range <>) of Point;

	function Header(w, h: Integer) return String;
	function Footer return String;
	function Polygon(pts: Points, width, color: String) return String;
end SVG;
