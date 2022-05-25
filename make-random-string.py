from random import randint

def func():
    n, k = map(int, input().split()) # n - количество строк, k - длина строки
    str = [] # список создаваемых строк
    for i in range(n):
        s = '' # вспомогательная строка
        for j in range(k):
            num = randint(33, 126) # символы на клавиатуре имеют коды от 33 до 126 (вкл)
            s += chr(num) # переводим код символа в символ, прибавляя его к вспомогательной строке
        str.append(s) # сохраняем строку
    for s in str:
        print(s)

func()

