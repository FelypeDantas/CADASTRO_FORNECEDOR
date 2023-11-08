     IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE003.
      *AUTHOR. FELYPE DANTAS DOS SANTOS.
      ******************************
      * CADASTRO DE FORNECEDOR     *
      ******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQFOR ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS FOR-CODIGO
                      ALTERNATE RECORD KEY IS FOR-NOME WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT ARQCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES.


      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".
       01 REGFOR.
          03 CADASTRO.
            05 FOR-CODIGO            PIC 9(06).
          03 FOR-TIPOCLIENTE         PIC X(01).
          03 FOR-CPF                 PIC 9(11).
          03 FOR-CNPJ                PIC 9(14).
          03 FOR-NOME                PIC X(30).
          03 FOR-APELIDO             PIC X(30).
          03 FOR-CEP                 PIC 9(08).
          03 FOR-LOGRADOURO          PIC X(24).
          03 FOR-NUMERO              PIC X(11).
          03 FOR-COMPLEMENTO         PIC X(24).
          03 FOR-BAIRRO              PIC X(40).
          03 FOR-CIDADE              PIC X(24).
          03 FOR-ESTADO              PIC X(02).
          03 FOR-TELEFONE            PIC 9(11).
          03 FOR-EMAIL               PIC X(33).
          03 FOR-CONTATO             PIC X(32).

      *
      *-----------------------------------------------------------------
       FD ARQCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP        PIC 9(08).
                03 CEP-LOGRA         PIC X(30).
                03 CEP-BAIRRO        PIC X(20).
                03 CEP-CIDADE        PIC X(20).
                03 CEP-UF            PIC X(02).
      *-----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-UPPERCASE   PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 ST-ERRO       PIC X(02) VALUE "00".


       01 DFOR-ESTADO            PIC X(15) VALUE SPACES.
       01 DFOR-TIPOCLIENTE       PIC X(20) VALUE SPACES.

       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 MENS          PIC X(50) VALUE SPACES.
       01 W-SEL         PIC 9(01) VALUE ZEROS.

      *--------------------------------------------
      *
       SCREEN SECTION.
       01  TELANOVA.
        05  LINE 01  COLUMN 01
               VALUE  "|-Cadastro de C".
           05  LINE 01  COLUMN 41
               VALUE  "liente -|".
           05  LINE 02  COLUMN 01
               VALUE  "|".
           05  LINE 02  COLUMN 41
               VALUE  "                                       |".
           05  LINE 03  COLUMN 01
               VALUE  "|Dados Pessoais|".

           05  LINE 04  COLUMN 01
               VALUE  "|  Informe o Tipo de Cliente :  -".
           05  LINE 04  COLUMN 41
               VALUE  "              Codigo:                  |".
           05  LINE 05  COLUMN 01
               VALUE  "|  CPF/CNPJ                  :".
           05  LINE 05  COLUMN 41
               VALUE  "                                       |".
           05  LINE 06  COLUMN 01
               VALUE  "|  Nome/Razao Social         :".
           05  LINE 06  COLUMN 41
               VALUE  "                                       |".
           05  LINE 07  COLUMN 01
               VALUE  "|  Apelido                   :".
           05  LINE 07  COLUMN 41
               VALUE  "                                       |".
           05  LINE 08  COLUMN 01
               VALUE  "|Endere".
           05  LINE 08  COLUMN 41
               VALUE  "co|".
           05  LINE 09  COLUMN 01
               VALUE  "|  CEP    :                           Lo".
           05  LINE 09  COLUMN 41
               VALUE  "gradouro  :                            |".
           05  LINE 10  COLUMN 01
               VALUE  "|  Numero :                           Co".
           05  LINE 10  COLUMN 41
               VALUE  "mplemento :                            |".
           05  LINE 11  COLUMN 01
               VALUE  "|  Bairro :".
           05  LINE 11  COLUMN 41
               VALUE  "                                       |".
           05  LINE 12  COLUMN 01
               VALUE  "|  Cidade :                           Es".
           05  LINE 12  COLUMN 41
               VALUE  "tado      :   -                        |".
           05  LINE 13  COLUMN 01
               VALUE  "|Contat".
           05  LINE 13  COLUMN 41
               VALUE  "o|".
           05  LINE 14  COLUMN 01
               VALUE  "|  Telefone :                         E-".
           05  LINE 14  COLUMN 41
               VALUE  "mail :                                 |".
           05  LINE 15  COLUMN 01
               VALUE  "|  Contato  :".
           05  LINE 15  COLUMN 41
               VALUE  "                                       |".
           05  LINE 16  COLUMN 01
               VALUE  "|---------------------------------------".
           05  LINE 16  COLUMN 41
               VALUE  "---------------------------------------|".
           05  LINE 17  COLUMN 01
               VALUE  "| Notificacoes :".
           05  LINE 17  COLUMN 41
               VALUE  "                                       |".
           05  LINE 18  COLUMN 01
               VALUE  "|---------------------------------------".
           05  LINE 18  COLUMN 41
               VALUE  "---------------------------------------|".
           05  LINE 19  COLUMN 01
               VALUE  "|Inform".
           05  LINE 19  COLUMN 41
               VALUE  "acoes                                  |".
           05  LINE 20  COLUMN 01
               VALUE  "|".
           05  LINE 20  COLUMN 41
               VALUE  "                                       |".
           05  LINE 21  COLUMN 01
               VALUE  "|".
           05  LINE 21  COLUMN 41
               VALUE  "                                       |".
           05  LINE 22  COLUMN 01
               VALUE  "|".
           05  LINE 22  COLUMN 41
               VALUE  "                                       |".
           05  LINE 23  COLUMN 01
               VALUE  "|".
           05  LINE 23  COLUMN 41
               VALUE  "                                       |".
           05  LINE 24  COLUMN 01
               VALUE  "|---------------------------------------".
           05  LINE 24  COLUMN 41
               VALUE  "---------------------------------------|".
           05  TFOR-TIPOCLIENTE
               LINE 04  COLUMN 32  PIC X(01)
               USING  FOR-TIPOCLIENTE.
           05  TDTIPOCLIENTE
               LINE 04  COLUMN 34  PIC X(20)
               USING  DFOR-TIPOCLIENTE.
           05  TFOR-CODIGO
               LINE 04  COLUMN 62  PIC 9(11)
               USING  FOR-CODIGO.
           05  TFOR-CPF
               LINE 05  COLUMN 32  PIC 999.999.999.999.99
               USING  FOR-CPF.
           05  TFOR-NOME
               LINE 06  COLUMN 32  PIC X(30)
               USING  FOR-NOME.
           05  TFOR-APELIDO
               LINE 07  COLUMN 32  PIC X(30)
               USING  FOR-APELIDO.
           05  TFOR-CEP
               LINE 09  COLUMN 13  PIC 99999.999
               USING  FOR-CEP.
           05  TFOR-LOGRADOURO
               LINE 09  COLUMN 52  PIC X(24)
               USING  CEP-LOGRA.
           05  TFOR-NUMERO
               LINE 10  COLUMN 13  PIC X(11)
               USING  FOR-NUMERO.
           05  TFOR-COMPLEMENTO
               LINE 10  COLUMN 52  PIC X(24)
               USING  FOR-COMPLEMENTO.
           05  TFOR-BAIRRO
               LINE 11  COLUMN 13  PIC X(40)
               USING  CEP-BAIRRO.
           05  TFOR-CIDADE
               LINE 12  COLUMN 13  PIC X(24)
               USING  CEP-CIDADE.
           05  TFOR-ESTADO
               LINE 12  COLUMN 53  PIC X(02)
               USING  CEP-UF.
           05  TDCLI-ESTADO
               LINE 12  COLUMN 56  PIC X(15)
               USING  DFOR-ESTADO.
           05  TFOR-TELEFONE
               LINE 14  COLUMN 15  PIC 9(11)
               USING  FOR-TELEFONE.
           05  TFOR-EMAIL
               LINE 14  COLUMN 47  PIC X(33)
               USING  FOR-EMAIL.
           05  TFOR-CONTATO
               LINE 15  COLUMN 15  PIC X(32)
               USING  FOR-CONTATO.



        01  TELALIMPAR.
           05  LINE 19  COLUMN 01
               VALUE  "                                              ".
           05  LINE 19  COLUMN 41
               VALUE  "                                              ".
           05  LINE 20  COLUMN 01
               VALUE  "                                               ".
           05  LINE 20  COLUMN 41
               VALUE  "                                               ".
           05  LINE 21  COLUMN 01
               VALUE  "                                               ".
           05  LINE 21  COLUMN 41
               VALUE  "                                              ".
           05  LINE 22  COLUMN 01
               VALUE  "                                               ".
           05  LINE 22  COLUMN 41
               VALUE  "                                               ".
           05  LINE 23  COLUMN 01
               VALUE  "                                               ".
           05  LINE 23  COLUMN 41
               VALUE  "                                               ".
           05  LINE 24  COLUMN 01
               VALUE  "                                               ".
           05  LINE 24  COLUMN 41
               VALUE  "                                               ".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      *-------------ABERTURA DO ARQUIVO -------------------
       R0.    OPEN I-O ARQFOR
           IF ST-ERRO NOT = "00"


            IF ST-ERRO = "30"
              OPEN OUTPUT ARQFOR
              MOVE "CRIANDO ARQUIVO CADASTRO DE FUNCIONARIOS " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQFOR
              GO TO INICIO
            ELSE
              IF ST-ERRO = "95"
                MOVE "ISAM NAO CARREGADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO CDAMIGOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       R0A.
           OPEN INPUT ARQCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           MOVE SPACES TO
           DFOR-TIPOCLIENTE DFOR-ESTADO FOR-TIPOCLIENTE
           FOR-NOME FOR-APELIDO FOR-LOGRADOURO FOR-NUMERO
           FOR-COMPLEMENTO FOR-BAIRRO FOR-CIDADE FOR-ESTADO
           FOR-EMAIL FOR-CONTATO.
           MOVE SPACES TO CEP-LOGRA CEP-BAIRRO CEP-CIDADE CEP-UF

           MOVE ZEROS TO FOR-CODIGO FOR-CPF FOR-CNPJ FOR-CEP
           FOR-TELEFONE
      *-------------VISUALIZACAO DA TELA ------------------

           DISPLAY TELANOVA.
      *----------ENTRADA DE DADOS---------------

       R2.

           ACCEPT TFOR-TIPOCLIENTE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R7.
       R2A.
           IF FOR-TIPOCLIENTE = "f" OR "F"
              MOVE "Pessoa Fisica" TO DFOR-TIPOCLIENTE
              MOVE "F" TO FOR-TIPOCLIENTE
           ELSE
            IF FOR-TIPOCLIENTE = "J" OR "j"
              MOVE "Pessoa Juridica" TO DFOR-TIPOCLIENTE
              MOVE "J" TO FOR-TIPOCLIENTE

             ELSE
              MOVE "F - Pessoa Fisica : J - Pessoa Juridica" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2.
           DISPLAY TDTIPOCLIENTE.
           DISPLAY TELALIMPAR.
           DISPLAY TELANOVA.




       R3.
           ACCEPT TFOR-CODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                  CLOSE ARQFOR
                  STOP RUN.
           IF FOR-CODIGO = SPACES
              MOVE "*** CODIGO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       LER-ARQFOR.
           READ ARQFOR
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R7A

                DISPLAY TELANOVA
                MOVE "*** CLIENTE JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADAMIGO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CLIENTE NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R4.
           ACCEPT TFOR-CPF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF FOR-CPF = SPACES
              MOVE "CPF NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           ACCEPT TFOR-NOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF FOR-NOME = SPACES
              MOVE "NOME NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
       R6.
           ACCEPT TFOR-APELIDO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
           IF FOR-APELIDO = SPACES
              MOVE " *** APELIDO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.

       R7.
           ACCEPT TFOR-CEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELANOVA
                   GO TO R7.
           IF FOR-CEP = 0
                 MOVE "*** CEP NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
       R7A.
           MOVE FOR-CEP TO CEP-NUMCEP.
           READ ARQCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELANOVA.




       R8.
           ACCEPT TFOR-NUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R7.
           IF FOR-NUMERO = SPACES
              MOVE " *** NUMERO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.
       R9.
           ACCEPT TFOR-COMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R7.
           IF FOR-COMPLEMENTO = SPACES
            MOVE " *** COMPLEMENTO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.

       R10.
           ACCEPT TFOR-TELEFONE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R9.
           IF FOR-TELEFONE = SPACES
            MOVE " *** TELEFONE NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R10.
       R11.
           ACCEPT TFOR-EMAIL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R10.
           IF FOR-EMAIL = SPACES
            MOVE " *** TELEFONE NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.
       R12.
           ACCEPT TFOR-CONTATO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R11.
           IF FOR-CONTATO = SPACES
            MOVE " *** CONTATO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R12.

           IF W-SEL = 1
            GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (17, 30) "DESEJA SALVAR (S/N) : ".
                ACCEPT (17, 55) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CLIENTE JA EXISTE,DADOS NAO GRAVADOS *" TO
                  MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (20, 18)
                     "N=NOVO REGISTRO | A=ALTERAR | E=EXCLUIR | S=SAIR:"
                ACCEPT (20, 67) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E" AND W-OPCAO NOT = "S"
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (20, 18) MENS
                IF W-OPCAO = "N"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
                ELSE
                   IF W-OPCAO = "S"
                      MOVE 1 TO W-SEL
                      GO TO ROT-FIM.
      *
       EXC-OPC.
                DISPLAY (17, 30) "DESEJA EXCLUIR   (S/N) : ".
                ACCEPT (17, 55) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQFOR RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FUNCIONARIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (17, 30) "DESEJA ALTERAR  (S/N) : ".
                ACCEPT (17, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY

                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.

                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM

                   GO TO R1.
                   DISPLAY TELALIMPAR.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQFOR ARQCEP.
           STOP RUN.
      *--------------------------------------------------------------
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (17, 21) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (17, 21) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NAO CARREGADO
      *    10 = FIM DA LEITURA ARQUIVO SEQUENCIAL
