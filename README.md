# Sujet23
Méthode de compression et de decompression de fichiers de Huffman.

```
Si vous voulez faire des choses en plus, compressez plutôt un gros fichier, sauvez le fichier compressé (ce qui inclut l'arbre (ou la table), de telle façon qu'un bit ne prenne vraiment qu'un bit), décompressez le, et vérifiez que vous avez bien retrouvé le fichier original.

Pour lire un fichier texte et le mettre dans une chaîne de caractères:
https://ocaml.org/learn/tutorials/file_manipulation.html
http://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml
http://camltastic.blogspot.fr/2008/09/tip-read-all-lines-from-file-most.html

Pour sauver un tableau de booléens et un arbre, vous les regroupez dans un "record" { arbre: tree; code: bool array}, et ensuite vous  utilisez le module Marshal
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html
pour le sauver sur disque et le relire.
```

Il est possible de trouver un corrigé dans le livre: Pierre Weis, Xavier Leroy. Le langage Caml, 2ème édition. Chapitre 13.
