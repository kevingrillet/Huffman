(*
 *	Projets Algorithmique et Complexite
 *	Sujet 23 - Compression de fichiers
 *		Programmer la methode de compression et de decompression de fichiers de Huffman.
 *		Voir http://en.wikipedia.org/wiki/Huffman_coding
 *
 *	@author:	Kevin GRILLET
 *	@date:		1/12/2016
 *	@version:	1
 *	@source(s):
 *				https://caml.inria.fr/pub/docs/manual-ocaml/libref/Char.html
 *				https://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html
 *				https://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html
 *				https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *				https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.Make.html
 *				https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
 *				https://ocaml.org/learn/tutorials/pointers.html
 *				https://ocaml.org/learn/tutorials/data_types_and_matching.html
 *
 *	@question(s):
 *				http://stackoverflow.com/questions/41138301/ocaml-string-histogram/
 *				http://stackoverflow.com/questions/9986310/concatenate-strings-in-ocaml-with-newline-between-them
 *)

module S = String;;
module HT = Hashtbl;;
module L = List;;
type 'e arbre =
	| Feuille of char
	| Noeud of ('e arbre * 'e arbre);;
module SM = Set.Make
	(struct
		type t = int * char arbre
		let compare = compare
	end);;

(*
 *	@see String, Hashtbl
 *	@param str String d'entree
 *	@return charFreqs Hashtbl lettre -> frequence
 *)
let histogram str =
	let charFreqs = HT.create 126 in
	S.iter (fun c ->
		let tmp =
			try HT.find charFreqs c
			with Not_found -> 0 in
		HT.replace charFreqs c (tmp+1)
	) str;
	charFreqs;;

(*
 *	@see Hashtbl, Set.Make
 *	@param charFreqs Hashtbl lettre -> frequence
 *	@return tree
 *)
let buildTree charFreqs =
	let freqs = HT.fold (fun c f acc -> (c,f)::acc) charFreqs [] in
	let fils = SM.of_list (L.map (fun (c,f) -> (f, Feuille c)) freqs) in
	let rec build arbres =
		let fFilsGauche,cFilsGauche = SM.min_elt arbres in
		let arbres' = SM.remove (fFilsGauche,cFilsGauche) arbres in
		if SM.is_empty arbres' then
			cFilsGauche
		else
			let fFilsDroit,cFilsDroit = SM.min_elt arbres' in
			let arbres'' = SM.remove (fFilsDroit,cFilsDroit) arbres' in
			let arbres''' = SM.add (fFilsGauche+fFilsDroit, Noeud (cFilsGauche,cFilsDroit)) arbres'' in
			build arbres''' in
			build fils;;

(*
 *	@see List, String
 *	@param code String vide
 *	@param racine arbre
 *)
let rec printTree code racine =
	match racine with
		| Feuille c ->
			Printf.printf "%c\t%s\n" c (String.concat "" (L.rev code));
		| Noeud (l, r) ->
			printTree ("0"::code) l;
			printTree ("1"::code) r;;

(*
 *	@see List, String, Hashtbl
 *	@param code String vide
 *	@param racine arbre
 *	@return hash Hashtbl lettre -> code
 *)
let buildHash code racine =
	let hash = HT.create 126 in
	let rec loop code racine =
		match racine with
			| Feuille c ->
				HT.replace hash c (String.concat "" (L.rev code));
			| Noeud (l, r) ->
				loop ("0"::code) l;
				loop ("1"::code) r;
	in loop code racine;
	hash;;

(*
 *	@see String, Hashtbl
 *	@param str String
 *	@param hash Hashtbl lettre -> code
 *	@return tmp String codee
 *)
let encode str hash =
	let tmp = ref "" in
	S.iter (fun c ->
		tmp:=S.concat "" [!tmp;(HT.find hash c)];
	) str;
	!tmp;;

(*
 *	@see String
 *	@param str String
 *	@param tree arbre
 *	@return tmp String decodee
 *)
let decode str tree =
	let tmp = ref "" in
	let tree' = ref tree in
	S.iter (fun c ->
		let rec loop c tree' =
			if c = '0' then
				match !tree' with
					| Feuille cRacine ->
						tmp:=S.concat "" [!tmp;(S.make 1 cRacine)];
						tree':=tree;
						loop c tree';
					| Noeud (filsGauche,_) ->
						tree':=filsGauche;
			else if c = '1' then
				match !tree' with
					| Feuille cRacine ->
						tmp:=S.concat "" [!tmp;(S.make 1 cRacine)];
						tree':=tree;
						loop c tree';
					| Noeud (_,filsDroit) ->
						tree':=filsDroit;
		in loop c tree';
	) str;
	match !tree' with
		| Noeud (_,_) ->
			failwith "Probleme de decodage";
		| Feuille cRacine ->
			tmp:=S.concat "" [!tmp;(S.make 1 cRacine)];
	!tmp;;

(* EXEMPLE 1:*)
let str = "TEXTE";;
let charFreqs = histogram str;;
HT.iter (fun x y -> Printf.printf "%c -> %d\n" x y) charFreqs;;
let tree = buildTree charFreqs;;
printTree [] tree;;
let hash = buildHash [] tree;;
HT.iter (fun x y -> Printf.printf "%c -> %s\n" x y) hash;;
let strencode = encode str hash;;
let strdecode = decode strencode tree;;

(* EXEMPLE 2:
let str = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";;
let tree = buildTree (histogram str);;
let strencode = encode str (buildHash [] tree);;
let strdecode = decode strencode tree;;
 *)
