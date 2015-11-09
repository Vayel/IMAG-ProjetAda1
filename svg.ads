-- Ce module facilite la création de fichier SVG.

package SVG is
  type Point is record
    x: Natural;
    y: Natural;
  end record;

  type Points is array (Integer range <>) of Point;

  procedure init;
	procedure Header(w, h: Integer);
	procedure Footer;
	procedure Polygon(pts: Points; width, color: String);
  function get_contents return String;
end SVG;
