-- Ce module facilite la création de fichier SVG.

package SVG is
  type Points is array (Integer range <>) of Integer;

	function Polygon(pts: Points) return String;
	function Header(w, h: Integer) return String;
	function Footer return String;
end SVG;
