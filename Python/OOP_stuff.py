# Perkulationsproblem in Python
import random  
  
# Aufgabe 3  
  
# True - durchlässig, False - fest  
# test 1 und 3 perkoliert, test 2 nicht  
test1 = [[True, False, True, False], [True, True, False, False], [False, True, True, False], [True, False, True, False]]  
test2 = [[True, False, True, False], [False, True, False, True], [True, False, True, False], [False, True, False, True]]  
test3 = [[False, False, False, False, True], [True, True, True, True, True], [True, False, False, False, False], [True, True, True, True, True], [False, False, False, False, True]]  
  
  
# (a)  
# Hilfsfunktion. Koordinaten einer Zelle werden auf Stack gespeichert und dann genutzt. Wenn die Zelle wahr ist, dann werden die Koordinaten links, rechts und unten  
# auf Stack gelegt. Wenn eine der Zellen in der letzte Zeile wahr ist, wird das Programm abgebrochen udn True geliefert, ansonsten False.  
def pathfinder(matrix, x, y):  
stack = [(x, y)]  
while stack:  
x, y = stack.pop()  
if matrix[y][x]:  
matrix[y][x] = None  
if y == len(matrix[0]) - 1:  
return True  
if x > 0:  
if not (x-1, y) in stack:  
stack.append((x - 1, y))  
if x < len(matrix[0]) - 1:  
if not (x+1, y) in stack:  
stack.append((x + 1, y))  
if not (x, y+1) in stack:  
stack.append((x, y + 1))  
return False  
  
# True, wenn Gitter perkoliert und False, wenn nicht.  
def perkoliert(grid):  
if not grid:  
return print("Empty matrix")  
for i in range(len(grid[0])):  
if pathfinder(grid, i, 0):  
return True  
return False  
  
  
# (b)  
# Generiert ein Zufallgitter mit Wahrscheinlichkeit p.  
def zufall_gitter(n, p):  
zufall_gitter = []  
for _ in range(n):  
row = []  
for _ in range(n):  
if random.random() < p:  
row.append(True)  
else:  
row.append(False)  
zufall_gitter.append(row)  
  
return zufall_gitter  
  
# Berechent die Wahrscheinlichkeit, dass ein nxn Gitter mit Wahrschienlichkeit p perkoliert. Dafür werden tests viele Testversuchen ausgeführt  
def perkolation_wahrscheinlichkeit(n, p, tests=500):  
  
if p == 0:  
return 0.0  
if p == 1:  
return 100.0  
  
anzahl_perkoliert = 0  
for _ in range(tests):  
gitter = zufall_gitter(n, p)  
if perkoliert(gitter):  
anzahl_perkoliert += 1  
return (anzahl_perkoliert / tests) * 100  
  
  
# (c)  
nums = [10, 20, 50]  
probs = [0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]  
  
for num in nums:  
res = []  
for pr in probs:  
res.append(perkolation_wahrscheinlichkeit(num, pr, 50000))  
print("Für", num, ":", res)

# Hangman
import random

# Aufgabe 3 (mit Kenntnissen)
# Liste der unterstützten Wörter
words = ["Mädchen", "Ehe", "Reibung", "Assistent", "Nebel", "Bewegung", "Fahrräder", "Wachs", "Sammlung", "Sofa", "Wut",
         "Kupfer", "Wetter", "Netz", "Meinung", "Ziege", "Laken", "Zwei", "Stern", "Schloss", "Ball", "Affäre",
         "Problem", "Einnahmen", "Wind", "Foto", "Masse", "Verbindung", "Sektor", "Argument", "Technologie",
         "Territorium", "Regenschirm", "Babys", "Kunde", "Hut", "Gänse", "Faden", "Kissen", "Eintritt"]

# Spielstart
# Buchstaben, die man noch erraten kann
secret_word = (random.choice(words)).upper()
# Das Wort, das man erraten muss
answer = secret_word
# Anzahl der Versuche
tries = len(secret_word) * 2
guessed_characters = ""
for c in range(0, len(secret_word)):
    guessed_characters = guessed_characters + "_"

# Mainloop
while True:
    # Nutzer gibt einen Buchstaben an. Wenn mehrere Buchstaben eingegeben werden, dann wird nur die erste akzeptiert.
    # Wenn es keine Angabe gibt, wird ein Leerzeichen als Eingabe gesetzt.
    try:
        guess = (input("Guess a character: ")[0]).upper()
    except IndexError:
        guess = " "

    # Wird ausgeführt, wenn eingegebene Buchstabe nicht in dem Wort enthalten ist
    # oder die Eingabe leer ist.
    if (guess == " ") or not (guess in secret_word):
        tries -= 1
        # Spiel beenden, wenn Spieler keine Versuche mehr hat
        if tries == 0:
            print("You have no more tries left. You lost.")
            print("The answer was: ")
            break

        print("Oops, wrong guess! Tries left:", tries)

    else:
        # Diese Zeile auskommentieren (while guess), um die Variante zu spielen, wo man jeden Buchstaben
        # separat erraten muss, auch wenn der Buchstaben mehrmals vorkommt
        while guess in secret_word:
            # Index von erratene Buchstabe kriegen
            index = secret_word.index(guess)

            # Erratene Buchstabe durch Leerzeichen ersetzen
            secret_word_list = list(secret_word)
            secret_word_list[index] = ' '
            secret_word = "".join(secret_word_list)

            # Erratene Buchstabe in angezeigte Buchstaben hinzufügen
            guessed_characters_list = list(guessed_characters)
            guessed_characters_list[index] = guess
            guessed_characters = "".join(guessed_characters_list)

    print(guessed_characters)

    if guessed_characters == answer:
        print("Yay, you won! Good game.")
        break

# Gambler Dillema
import random

# Aufgabe 2. (b)


def start_up(r):
    if r <= 1:
        raise Exception
    a = 1
    b = r
    while True:
        c = (a+b) / 2
        if (c*c) == r:
            return c
        elif (c*c) > r:
            b = c
        elif (c*c) < r:
            a = c
        if (a != 1) and (r-(a*a)) < 0.0001:
            return a


"""
print("Test for 2:", start_up(2))
print("Test for 5:", start_up(5))
print("Test for 6:", start_up(6))
print("Test for 54:", start_up(54))
print("Test for 9:", start_up(9))
"""


# Aufgabe 2. c


def start_up_extended(r):
    if r <= 1:
        a = 0
        b = r + 1
    else:
        a = 1
        b = r

    while True:
        c = (a+b) / 2
        if (c*c) == r:
            return c
        elif (c*c) > r:
            b = c
        elif (c*c) < r:
            a = c
        if (a != 0 and a != 1) and (r-(a*a)) < 0.0001:
            return a


"""
print("Test for 2:", start_up_extended(1))
print("Test for 5:", start_up_extended(5))
print("Test for 6:", start_up_extended(6))
print("Test for 54:", start_up_extended(54))
print("Test for 9:", start_up_extended(9))
"""


# Aufgabe 3. a
# Als Funktion, um Statistik zu sammeln


def gambler():
    # Als Eingabe werden nur Integers akzeptiert, da die Teilnahme im Spiel
    # für jede Runde 1 Euro kostet.

    # Bank-Variable A
    a = int(input("Wie viel Euro hat Bank zur Verfügung? "))
    # Hannelore-Variable B
    b = int(input("Wie viel Euro hat Hannelore zur Verfügung? "))

    if a <= 0:
        print("Bank hat momentan kein Geld zur Verfügung und kann deswegen nicht spielen")
        exit()
    if b <= 0:
        print("Hannelore hat momentan kein Geld zur Verfügung und kann deswegen nicht spielen")
        exit()

    while (a != 0) and (b != 0):
        # Simuliert den Wurf einer Münze. 1 steht für Zahl und 0 für Kopf
        random_number = random.randint(0, 1)
        if random_number == 0:
            b += 1
            a -= 1
        if random_number == 1:
            a += 1
            b -= 1

    if a == 0:
        # return True
        print("Bank hat verloren")
        exit()
    if b == 0:
        # return False
        print("Hannelore hat verloren")
        exit()


"""
Funktion und Output, um mehrere Tests durchzuführen

def repeater(bank, hannelore):
    wins = 0
    tests = 5000
    for _ in range(tests):
        bo = gambler(bank, hannelore)
        if bo:
            wins += 1
    return wins / tests


print(repeater(1000, 40))
"""

gambler()

# Rotate matrices
from copy import deepcopy


# Aufgabe 1 - (a)


def sprod(l, k):
    # Fehler, falls Listen unterschiedliche Länge haben.
    if len(l) != len(k):
        raise Exception("Lists have different length, operation not possible.")

    if not l and k:
        return 0.0

    results = []
    for i in range(0, len(l)):
        results.append(l[i] * k[i])
    return sum(results)


# print(sprod([1, 2, 3.0, 4.8, 7], [1, 2, 3.0, 4.8, 7]))
# print(sprod([], []))
# print(sprod([1, 2, 3.0, 4.8, 7], [1, 2, 3.0]))


# Aufgabe 1 - (b)
def histogram(s):
    memory = {}
    for e in s:
        if e in memory:
            memory[e] += 1
        else:
            memory[e] = 1
    return memory


# print(histogram("Mississippi"))
# print(histogram("Banana Manana"))
# print(histogram(""))


# Aufgabe 1 - (c)
def concatenate(l):
    result = []
    for sub in l:
        for e in sub:
            result.append(e)
    return result


# print(concatenate([[1, 2, 3.0, 4.8, 7], [1, 2, 3.0, 4.8, 7], [1, 2, 3.0, 4.8, 7]]))
# print(concatenate([[], []]))
# print(concatenate([]))


# Aufgabe 2 - (a)
def sort_ketchup_help(l, result):
    if not l:
        return result
    smallest = float("inf")
    for e in l:
        if e < smallest:
            smallest = e
    result.append(smallest)
    l.remove(smallest)
    return sort_ketchup_help(l, result)


def sort_ketchup(l):
    return sort_ketchup_help(l, [])


# print(sort_ketchup([8, 7, 6, 5, 4, 18, 2, 1]))


# Aufgabe 2 - (b)

"""
Nehme die erste Flasche und vergleiche die mit allen anderen, ob es eine kleinere Flasche gibt.
Stelle die kleinste Flasche am Anfang der Reihe und sortiere die restlichen Flaschen.
(so oft wie es nicht sortierte Flaschen vorhanden sind).
"""

def sort_ketchup_iterative(l):
    if not l:
        return []
    results = []
    for i in range(len(l)):
        smallest = float("inf")
        for e in l:
            if e < smallest:
                smallest = e
        results.append(smallest)
        l.remove(smallest)
    return results


# print(sort_ketchup_iterative([8, 7, 6, 5, 4, 18, 2, 1]))
# print(sort_ketchup_iterative([]))


# Aufgabe 3 - mit Programmierkenntnissen

# a

"""
Man kann ein Spielfeld als eine Liste, die drei Listen enthält, je mit 3 Positionen, darstellen:

[
 [0, 0, 0]
 [0, 0, 0]
 [0, 0, 0]
]

0 für leer
1 für Spieler 1
2 für Spieler 2

"""

# (b)

"""
rotate_counter_90:
1 2 3     3 6 9
4 5 6 --> 2 5 8
7 8 9     1 4 7
"""


def rotate_counter_90(board):
    return [[board[i][i2] for i in range(len(board))] for i2 in range(len(board[0])-1, -1, -1)]


"""
mirror:
1 2 3     1 4 7
4 5 6 --> 2 5 8
7 8 9     3 6 9
"""


def mirror(board):
    return [[board[i][i2] for i in range(len(board))] for i2 in range(len(board[0]))]

# Es werden alle möglichen Rotationen und ihre Spiegelungen durchegagnen.
# Wenn man ein Feld in anderen überführen kann, dann wird True ausgegeben,
# ansonsten False


def symmetric_board(board, board_2):
    if board == board_2:
        return True

    rotated_boards = [board]
    for _ in range(3):
        rotated_boards.append(rotate_counter_90(rotated_boards[-1]))

    mirrored_boards = [mirror(rotated_board) for rotated_board in rotated_boards]

    for rotated_board in rotated_boards:
        if rotated_board == board_2:
            return True

    for mirrored_board in mirrored_boards:
        if mirrored_board == board_2:
            return True

    return False

# (d)


def boardstate(board):
    # Fehlerbehandlung, wenn Feld nicht richtig eingegeben wurde
    if len(board) != 3 or any(len(row) != 3 for row in board):
        print(board)
        raise ValueError("Invalid board dimensions")
    # Alle mögliche Kombinationen von Stellen, mit denen man gewinnt.
    win_combs = [((0, 0), (0, 1), (0, 2)), ((1, 0), (1, 1), (1, 2)), ((2, 0), (2, 1), (2, 2)),
                 ((0, 0), (1, 0), (2, 0)), ((0, 1), (1, 1), (2, 1)), ((0, 2), (1, 2), (2, 2)),
                 ((0, 0), (1, 1), (2, 2)), ((0, 2), (1, 1), (2, 0))]
    for comb in win_combs:
        if board[comb[0][0]][comb[0][1]] == board[comb[1][0]][comb[1][1]] == board[comb[2][0]][comb[2][1]]:
            if board[comb[0][0]][comb[0][1]] == 1:
                # print("Spieler 1 hat gewonnen!")
                return 1
            elif board[comb[0][0]][comb[0][1]] == 2:
                # print("Spieler 2 hat gewonnen!")
                return 2
            else:
                continue
    if sum(row.count(0) for row in board) == 0:
        # print("Unentschieden.")
        return 0
    # print("Das Spiel ist noch aktiv!")
    return -1


# boardstate([[2, 1, 2], [2, 1, 1], [1, 2, 1]])


# (c)
# Es werden alle mögliche Nachfolgerkonfigurationen ermittelt.
# Wenn ein Feld erreicht wird, wo einer der beiden Spieler gewonnen hat,
# hört die Ermittlung auf.


def succ_board(board, player):
    results = []
    empty_cells = sum(row.count(0) for row in board)
    if empty_cells == 0:
        raise Exception("The board is already full")
    succ_boards = []
    for i in range(3):
        for i2 in range(3):
            if board[i][i2] == 0:
                new_board = deepcopy(board)
                new_board[i][i2] = player
                succ_boards.append(new_board)
    for i1, matrix1 in enumerate(succ_boards):
        for i2, matrix2 in enumerate(succ_boards):
            if i1 == i2:
                continue
            if symmetric_board(matrix1, matrix2):
                succ_boards.remove(matrix2)
    while sum(row.count(0) for row in succ_boards[-1]) != 0:

        if player == 1:
            player = 2
        else:
            player = 1

        succ_boards_2 = []

        for brd in succ_boards:
            if boardstate(brd) != -1:
                continue

            for i in range(3):
                for i2 in range(3):
                    if brd[i][i2] == 0:
                        new_board = deepcopy(brd)
                        new_board[i][i2] = player
                        succ_boards_2.append(new_board)

            for i1, matrix1 in enumerate(succ_boards_2):
                for i2, matrix2 in enumerate(succ_boards_2):
                    if i1 == i2:
                        continue
                    if symmetric_board(matrix1, matrix2):
                        succ_boards_2.remove(matrix2)
        for matrixx in deepcopy(succ_boards):
            results.append(matrixx)
        succ_boards = deepcopy(succ_boards_2)

    return results


# print(succ_board([[1, 0, 0], [0, 0, 0], [0, 0, 0]], 2))
# print(succ_board([[1, 0, 0], [0, 2, 1], [0, 0, 0]], 2))


# (e)
def best_strategy():
    case_0 = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    best_start = (case_0, 0.0)
    results = []
    cases = [
        [[1, 0, 0], [0, 0, 0], [0, 0, 0]],
        [[0, 0, 0], [0, 1, 0], [0, 0, 0]],
        [[0, 1, 0], [0, 0, 0], [0, 0, 0]]
            ]
    for pcase in cases:
        games = succ_board(pcase, 2)
        wins = sum(1 for game in games if boardstate(game) == 1)
        games_played = sum(1 for game in games if boardstate(game) != -1)
        win_rate = wins / games_played

        results.append((pcase, win_rate))

    for result in results:
        if result[1] > best_start[1]:
            best_start = result
    print(f"Der beste erster Zug ist {best_start[0]}. So gewinnt man mit Wahrscheinlichkeit von {best_start[1] * 100} Prozent.")
    return results


# print(best_strategy())

"""
Die beste Strategie für Spieler 1 ist:

[
 [0, 0, 0]
 [0, 1, 0]
 [0, 0, 0]
]

"""

# Fibonacci und Mensakartenproblem
import time  
  
# Aufgabe 1.  
  
# (a)  
  
def fibonacci_rekursiv(n):  
    if n == 0 or n == 1:  
        return 1  
    else:  
        return fibonacci_rekursiv(n - 1) + fibonacci_rekursiv(n - 2)  
  
  
# (b)  
  
start = time.time()  
print(fibonacci_rekursiv(35))  
end = time.time()  
  
total = end - start  
print(f"Zeit für a: {total}")  
  
# (c)  
  
  
def fibonacci_rekursiv_memory(n, memory={0: 1, 1: 1}):  
    if n in memory:  
        return memory[n]  
    else:  
        memory[n] = fibonacci_rekursiv_memory(n-1, memory) + fibonacci_rekursiv_memory(n-2, memory)  
        return memory[n]  
  
  
start = time.time()  
print(fibonacci_rekursiv_memory(35))  
end = time.time()  
  
total = end - start  
print(f"Zeit für b: {total}")  
  
  
# (d)  
  
def fibonacci_iterativ(n):  
    memory = [1, 1]  
    counter = 2  
    if n == 0 or n == 1:  
        return 1  
    while True:  
        memory.append(memory[counter - 1] + memory[counter - 2])  
        if counter == n:  
            return memory[counter]  
        counter += 1  
  
  
start = time.time()  
print(fibonacci_iterativ(35))  
end = time.time()  
  
total = end - start  
print(f"Zeit für d: {total}")  
  
# e  
  
  
def fibonacci_iterativ_modified(n):  
    memory = [1, 1]  
    counter = 2  
    if n == 0 or n == 1:  
        return 1  
    while True:  
        memory.append(memory[0] + memory[1])  
        memory.pop(0)  
        if counter == n:  
            return memory[1]  
        counter += 1  
  
  
start = time.time()  
print(fibonacci_iterativ_modified(35))  
end = time.time()  
  
total = end - start  
print(f"Zeit für e: {total}")  
  
  
# Aufgabe 2  
  
# (a)  
  
def auswahl(a, p, B, wahl=[]):  
    # Hilfsfunktion, um Mensakartenproblem zu erweitern.  
    # Anker. Wenn es nur eine Zeile in Tabelle übrig bleibt, dann soll    # eine mögliche Auswahl ausgegeben    if len(a) == 1:  
        return wahl  
    for i in range(len(p)):  
        if B - p[i] >= 0 and a[0][B-p[i]]:  
            # mögliche Speisen werden zur Kombination  
            # hinzugefügt, wenn man diese noch kaufen kann.            wahl.append(p[i])  
            B = B-p[i]  
            p.remove(p[i])  
            a = a[1:]  
            return auswahl(a, p[:], B, wahl)  
    return wahl  
  
  
def mensakarte_modified_a(B, p):  
    # Die erweiterte Funktion, die eine mögliche Auswahl zurückgibt.  
    n = len(p)  
    a = [([False] * (B + 1)) for _ in range(0, n+1)]  
    a[n][0] = True  
    for i in range(n-1, -1, -1):  
        for b in range(0, B+1):  
            if p[i] <= b and a[i + 1][b - p[i]]:  
                a[i][b] = True  
            else:  
                a[i][b] = a[i+1][b]  
    return auswahl(a, p, B)  
  
  
print(mensakarte_modified_a(13, [6, 7, 8, 9, 2, 10, 11, 3]))  
  
  
# (b)  
  
def mensakarte_modified_b(B, p):  
    # Erweiterung, um weniger Speicher zu nutzen.  
    n = len(p)  
    a = [([False] * (B + 1)) for _ in range(0, 2)]  
    a[1][0] = True  
    for i in range(n-1, -1, -1):  
        for b in range(0, B+1):  
            if p[i] <= b and a[1][b - p[i]]:  
                a[0][b] = True  
            else:  
                a[0][b] = a[1][b]  
        # Es werden nur die Zwischenergebnisse benötigt, also  
        # werden die übrigen Zeilen gelöscht        a.pop(1)  
        a.insert(0, ([False] * (B + 1)))  
    return a[1][B]  
  
  
print(mensakarte_modified_b(13, [6, 7, 8, 9, 2, 10, 11, 3]))  
  
  
# (c)  
  
  
def mensakarte_lustgewinn(B, p):  
    # mögliche Kombinationen werden gleich in die  
    # Lustgewinne umgewandelt und nur der größte Lustgewinn    # wird ausgegeben    combos = []  
    for i, elem in enumerate(p):  
        if B - elem[0] >= 0:  
            q = p.copy()  
            q.remove(p[i])  
            # Umwandlung in Lustgewinn  
            combos.append(mensakarte_lustgewinn(B - elem[0], q) + elem[1])  
    if not combos:  
        # Wenn es keine möglichen Kombinationen gibt, dann ist der Lustgewinn 0  
        return 0  
    return max(combos)  
  
  
print(mensakarte_lustgewinn(33, [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12), (13, 14), (15, 15)]))

# Insertion Sort, aber mit binäre Suche von richtige Position
def binäres_insertion_sort(liste):  
    if len(liste) <= 1:  
        return  
    for i in range(1, len(liste)):  
        key = liste[i]  
        left = 0  
        right = i - 1  
  
        # Binäre Suche, um die Einfügeposition zu finden  
        while left <= right:  
            mid = (left + right) // 2  
            if liste[mid] > key:  
                right = mid - 1  
            else:  
                left = mid + 1  
  
        # Verschiebe Elemente nach rechts  
        for j in range(i, left, -1):  
            liste[j] = liste[j - 1]  
  
        # Füge das Element an der Einfügeposition ein  
        liste[left] = key  
  
    return liste  
  
#Testlauf  
#print(binäres_insertion_sort([9,7,8,5,4,6,2,3,1]))

