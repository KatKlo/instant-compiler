# Instant Compiler

#### Katarzyna Kloc (kk429317)

## Kompilacja i uruchomienie
Polececnie `make` tworzy w korzeniu pliki wykonywalne
- kompilator języka Instant do JVM - insc_jvm
- kompilator języka Instant do JVM - insc_llvm

Polecenie `make cleanDist` usuwa niepotrzebne pliki stowrzone w procesie kompilacji. 
Polecenie `make clean` usuwa stworzone pliki wykonywalne.

## Działanie
Kompilatory przyjmują na wejściu plik z kodem źródłowym języka Instant i tworzą odpowiednio pliki pliki:
- insc_jvm:
    - `<fileName>.j` - plik z kodem bajtowym JVM
    - `<fileName>.class` - plik wykonywalny JVM
- insc_llvm:
    - `<fileName>.ll` - plik z kodem LLVM
    - `<fileName>` - plik wykonywalny LLVM

Specyfika działania kompilatorów:
- Wychwytywane są błędy składniowe
- Ignorowane są błędy typu "Runtime" jak np. dzielenie przez 0, przekroczenie zakresu typu int32
- Pozwalają na korzystanie z niezadeklarowanych zmiennych

### Implementacja kompilatora
- Kompilator został napisany w języku Haskell przy wykorzystaniu monad Writer i State
- Do generowania kodu bajtowego JVM wykorzystano bibliotekę [jasmin](https://jasmin.sourceforge.net)
- Do generowania kodu LLVM wykorzystano bibliotekę [llvm-as](https://llvm.org/docs/CommandGuide/llvm-as.html)
- Sposób implementacji inspirowany własnym rozwiązaniem zadania interpretera w ramach [JPP](https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&kod=1000-216bJPP)

### Struktura projektu
- `src/` - pliki źródłowe kompilatora
  - `src/Grammar/` - pliki wygenerowane przy użyciu [bnfc](https://bnfc.digitalgrammars.com) na podstawie dostarczonego pliku `Instant.cf`
  - `src/JvmGenerator/` - pliki generatora kodu w postaci JVM
  - `src/LlvmGenerator/` - pliki generatora kodu w postaci LLVM
- `lib/` - jar z biblioteką [jasmina](https://jasmin.sourceforge.net)
