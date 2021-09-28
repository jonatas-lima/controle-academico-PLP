main:- 
    write("Bem-Vindo(a)!\nPara acessar o controle, faça login:\n\n"),
    login(),
    halt.

login:-
    write("Digite sua matrícula:"),
    read(Matricula).
