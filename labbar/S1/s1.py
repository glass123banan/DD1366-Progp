def dna():          # uppgift 1
    # ^  -> början av strängen
    # $  -> slutet av strängen, alltså att hela strängen matchar
    # [ACGT] -> bara en av A, C, G, T får vara med
    # + -> det måste förekomma minst en gång, alltså inte tillåta en tom sträng
    return r'^[ACGT]+$'


def sorted():       # uppgift 2
    # ^  -> början av strängen
    # 9* -> 0 eller fler 9:or, vi tillåter alltså inga 9 oxå
    # samma sak för alla siffror (däremot får då inte ex en 6a förekomma innan en 7a)
    # $  -> slutet av strängen, alltså att hela strängen matchar
    return r'^9*8*7*6*5*4*3*2*1*0*$'


def hidden1(x):     # uppgift 3
    # indata x är strängen som vi vill konstruera ett regex för att söka efter
    # ^  -> början av strängen
    # .* -> 0 eller fler av vilken karaktär som helst . = vilken karaktär som helst, * = 0 eller fler
    # x  -> den sträng vi vill söka efter
    # .* -> 0 eller fler av vilken karaktär som helst ännu en gång
    # $  -> slutet av strängen, alltså att hela strängen matchar
    return '^.*' + x + '.*$'


def hidden2(x):     # uppgift 4
    # indata x är strängen som vi vill konstruera ett regex för att söka efter
    # Samma som ovan, fast med join. 
    # Ifall vi tar ex progp så gör join att det blir:
    # ^.*p.*r.*o.*g.*p.*$, alltså sätter man ihop varje karaktär med .*
    return '^.*' + '.*'.join(x) + '.*$'

def equation():     # uppgift 5
    sign       = r'[+\-]?' # Signalerar om FÖRSTA talet är pos/neg
    number     = r'\d+' # ett eller fler digits (nummer 0-9)
    operator   = r'[+\-*/]' # Operator: backslash på minus pga annars ascii + till *
    
    # ett tal med valfri ledande plus eller minus
    basic_expr = sign + number
    
    # operator följt av ett tal, upprepade noll eller flera gånger
    ops_expr   = r'(?:' + operator + number + r')*'
    
    # = följt av ett nytt uttryck (samma mönster som ovan), som är valfritt
    eq_expr    = r'(?:=' + basic_expr + ops_expr + r')?'
    
    return r'^' + basic_expr + ops_expr + eq_expr + r'$'

def parentheses():
    inner  = r"(\(\))*"                     # matchar "()" upprepade gånger
    level4 = r"(\(" + inner + r"\))*"       # matchar "(" följt av inner, noll eller flera gånger avslutad med ")"
    level3 = r"(\(" + level4 + r"\))*"      # nästa nivå
    level2 = r"(\(" + level3 + r"\))*"      # nästa nivå
    level1 = r"(\(" + level2 + r"\))"       # yttersta nivån
    regex  = r"^(" + level1 + r")+$"        # hela uttrycket
    return regex


def sorted3():      # uppgift 7
    group1 = r"01[2-9]"
    group2 = r"[0-1]2[3-9]"
    group3 = r"[0-2]3[4-9]"
    group4 = r"[0-3]4[5-9]"
    group5 = r"[0-4]5[6-9]"
    group6 = r"[0-5]6[7-9]"
    group7 = r"[0-6]7[8-9]"
    group8 = r"[0-7]89"

    return r"^[0-9]*(" + "|".join([group1, group2, group3, group4, group5, group6, group7, group8]) + r")[0-9]*$"