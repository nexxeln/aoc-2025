{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ocaml
  ];

  shellHook = ''
    echo "🎄 Advent of Code 2025"
    echo ""

    # Helper function to run solutions
    run() {
      if [ $# -ne 2 ]; then
        echo "Usage: run <day> <part>"
        echo "Example: run 1 2  (runs day 1 part 2)"
        return 1
      fi

      local day=$(printf "%02d" $1)
      local part=$2
      local file="day-$day/$part.ml"

      if [ ! -f "$file" ]; then
        echo "Error: $file not found"
        return 1
      fi

      echo "Running Day $1 Part $part..."
      cd "day-$day" && ocaml "$part.ml" && cd ".."
    }

    echo "Commands available:"
    echo "  run <day> <part>  - Run a solution (e.g., run 1 2)"
  '';
}
