Antoine Descamps, Aurélien Péden, Loïc Bernard, Maxime Huyghe  
Tuteur : Denis Monnerat
# Cahier des charges – Langage de programmation éducatif et compilateur

Pour ce projet, nous allons concevoir un langage de programmation à visée 
éducative et réaliser un compilateur/interpréteur pour ce langage.


# Analyse fonctionnelle
Échelle MoSCoW : **M**ust **S**hould **C**ould **W**ould

**M** Le langage sera raisonablement facile à utiliser.

**M** Le langage pourra être interprété.

**M** Le langage aura un système de type statique plutôt simple.

**M** Le langage aura une syntaxe simple, avec du sucre syntaxique en modération. 
Example :

    fun f(x, y) is
       y := x + 1
       return x + y
    end
    
    while a < 10 do
       the_thing()
    end
    
    for i in 0..10 do
       print(i)
    end
    
    foo(bar) = bar.foo()
    
    
**S** Le compilateur produira des messages d'erreur très utiles (type Rust ou Elm)

**S** Le compilateur incluera un formateur automatique, les débutants ayant tendance
à écrire du code difficile à lire.

**S** La mémoire sera gérée par un ramasse-miette.

**S** Le langage disposera d'une bibliothèque standard basique.
 Tableaux (+ strings), Autres types de base, IO console.

**C** L'utilisateur devra pouvoir exécuter son code rapidement, la compilation
/pré-interprétation devra donc être rapide.

**C** Le langage pourra être compilé (bytecode, C, ou asm).

**W** Le langage disposera d'une bibliothèque standard assez remplie.
 IO fichiers, Réseau , Multithreading.


# Analyse technique
Nous nous inspirerons de techniques d'écriture de compilateur existantes ainsi
que de livres sur le sujet afin de ne pas tout réinventer.


Le compilateur sera écrit en Rust, un langage un peu difficile à apprendre,
mais qui dispose des avantages suivants :
    - Un système de type puissant très propice à l'écriture de compilateurs
    - Des exécutables très rapides (niveau C[++]) tout en étant plutôt haut niveau


Nous écrirons probablement un parseur descendant récursif à la main.


La structure générale sera :
Source =Lexing=> Tokens =Parsing=> Arbre syntaxique (=Opti=> Arbre syntaxique)\**
Ensuite soit interprétation de l’arbre, soit écriture de C/assembleur/bytecode

# Organisation et méthode
Il a été difficile de s'organiser puisqu'aucun de nous ne sait exactement comment développer un langage de programmation. Nous avons donc dû nous renseigner sur différents sites, comme : 
https://tomassetti.me/how-to-create-programming-language/, 
ou encore https://www.freecodecamp.org/news/the-programming-language-pipeline-91d3f449c919/ ...

Après que nous nous sommes renseignés, nous avons pu prendre quelques décisions. Le langage que nous allions créer devait être interprété et non pas compilé, car il nous semblait que ce serait plus simple et que le projet était déjà suffisamment complexe en l'état.
L'interprétation est un procédé moins efficace mais est plus simple à mettre en place. S'il nous reste du temps une fois l'interpréteur développé, peut-être que nous tenterons de compiler le langage en C ou en bytecode Java.

Nous avons choisi d'utiliser le langage Rust sur les recommandations de Maxime qui avait déjà un peu d'expérience dans l'écriture de compilateurs et d'interpréteurs. Nous avons hésité avec le langage Python qui présente des fonctionnalités de parsing et de traitement de chaînes de caractères avancées.

Nous allons continuer de consulter différentes sources (youtube, livres, tutoriels, ...) afin d'en apprendre plus sur le développement d'un langage de programmation. Il est difficile d'établir un planning précis puisque comme dit précédemment nous en savons peu sur la manière dont il faut s'y prendre, et que nous ne pouvons pas nous aider de certaines notions apprises en cours comme par exemple UML, les diagrammes de classes, etc... Nous avons malgré tout essayé d'établir un planning et nous avons dressé un diagramme de Gantt pour établir globalement les tâches à réaliser.

Nous avons décidé que d'ici mi-janvier il faudrait qu'on dispose d'un langage, aussi simple soit-il, pouvant être interprété grâce à un programme en Rust. A partir de là il nous sera plus facile d'explorer de nouvelles pistes et de mieux définir le langage en lui ajoutant des fonctionnalités. 
