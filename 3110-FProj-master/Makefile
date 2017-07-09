compile:
					ocamlbuild -pkgs lablgtk2,unix gamegui.byte && ./gamegui.byte
clean:
					ocamlbuild -clean
game:
					ocamlbuild -pkgs lablgtk2,unix main.byte && ./main.byte
test:
					ocamlbuild -pkgs oUnit,lablgtk2 test.byte && ./test.byte
