IDENTIFICATION DIVISION.
    PROGRAM-ID.projetCobol.
    AUTHOR.AndyArnaudEtienneAldvine.
    DATE-WRITTEN.200218.
    
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	
    SELECT fseances ASSIGN TO "seances.dat"
    ORGANIZATION indexed
    ACCESS IS dynamic
    RECORD KEY fsea_id
    ALTERNATE RECORD KEY fsea_date WITH DUPLICATES
    ALTERNATE RECORD KEY fsea_idfilm WITH DUPLICATES
    FILE STATUS IS fsea_stat.
    
    SELECT fsalles ASSIGN TO "salles.dat"
    ORGANIZATION indexed
    ACCESS IS dynamic
    RECORD KEY fsal_num
    FILE STATUS IS fsal_stat.
    
    SELECT ffilms ASSIGN TO "films.dat"
    ORGANIZATION indexed
    ACCESS IS dynamic
    RECORD KEY ff_id
    ALTERNATE RECORD KEY ff_genre WITH DUPLICATES
    FILE STATUS IS ff_stat.
    
    SELECT fclients ASSIGN TO "clients.dat"
    ORGANIZATION indexed
    ACCESS IS dynamic
    RECORD KEY fc_mail
    FILE STATUS IS fc_stat.
    
    SELECT freservation ASSIGN TO "reservation.dat"
    ORGANIZATION indexed
    ACCESS IS dynamic
    RECORD KEY fr_num
    ALTERNATE RECORD KEY fr_idseance WITH DUPLICATES
    FILE STATUS IS fr_stat.
    
    
DATA DIVISION.

    FILE SECTION.
    FD fseances.
    01 seaTampon.
        02 fsea_id PIC 9(4).
        02 fsea_date.
			     03 fsea_jour PIC 9(2).
			     03 fsea_mois PIC 9(2).
			     03 fsea_annee PIC 9(4).
		    02 fsea_horaire.
			     03 fsea_minute PIC 9(2).
			     03 fsea_heure PIC 9(2).
        02 fsea_numsalle PIC 9(4).
        02 fsea_idfilm PIC 9(4).			
        02 fsea_typedif PIC 9.
        
    FD fsalles.
    01 salTampon.
        02 fsal_num PIC 9(4).
        02 fsal_nbplace PIC 9(3).
        
    FD ffilms.
    01 filmTampon.
        02 ff_id PIC 9(4).
        02 ff_titre PIC A(50).
        02 ff_genre PIC A(20).
        02 ff_annee PIC 9(4).

    FD fclients.
    01 clieTampon.
        02 fc_mail PIC A(500).    
        02 fc_prenom PIC A(30).
        02 fc_datedeb.
           03 fc_annee PIC 9(4).
           03 fc_mois PIC 9(2).
           03 fc_jour PIC 9(2).
        02 fc_duree PIC 9(2).
        
    FD freservation.
    01 reserTampon.
        02 fr_num PIC 9(4).    
        02 fr_idseance PIC 9(4).    
        02 fr_place PIC 9(3).    
        02 fr_montant PIC 9(4).    
        02 fr_placeAbonne PIC 9(2).   

WORKING-STORAGE SECTION.

    *> variables de compte rendu
    77 fsea_stat PIC 9(2).
    77 fsal_stat PIC 9(2).
    77 ff_stat PIC 9(2).
    77 fp_stat PIC 9(2).
    77 fc_stat PIC 9(2).
    77 fr_stat PIC 9(2).
    
    *> variables constantes
    77 WtarifAdulte PIC 99 VALUE 7.
    77 WtarifEnfant PIC 99 VALUE 3.
    77 WtarifReduc PIC 99 VALUE 4.
    77 Wtarif3D PIC 99 VALUE 1.
    
    *> variable autre
    77 Wmenu PIC 9(2).
    
    
    
    *> variables du fichier seances.dat
    77 WidS PIC 9(4).
    77 WnumsalleS PIC 9(4).
    77 WidfilmS PIC 9(4).
    77 WtypedifS PIC 9(2).
    77 WminuteS PIC 9(2).
    77 WheureS PIC 9(2).
    77 WjourS PIC 9(2).
    77 WmoisS PIC 9(2).
    77 WanneS PIC 9(4).
    
    *> variables du fichier salles.dat
    77 WnumS PIC 9(4).
    77 WnbplaceS PIC 9(3).
    
    *> variables du fichier films.dat
    77 WidF PIC 9(4).
    77 WtitreF PIC A(50).
    77 WgenreF PIC A(20).
    77 WanneeF PIC 9(4).
    
    *> variables du fichier clients.dat
    77 WmailC PIC A(250).    
    77 WprenomC PIC A(30).
    77 WdatedebC PIC X(15).
    77 WdureeC PIC 9(2).
    
    *> variables du fichier reservations.dat
    77 WnumR PIC 9(4).    
    77 WidseanceR PIC 9(4).    
    77 WplaceR PIC 9(3).    
    77 WmontantR PIC 9(4).
    77 Wplace_abonneR PIC 9(2).
    
    *> variable de la fonction ajout_seances
    77 Wanneeok PIC 9(2).
    01 WdateActu. *> utilisé dans recherche_seance aussi
    	02 Wannee PIC 9(4).
    	02 Wmois PIC 9(2).
      02 Wjour PIC 9(2).
      02 Wheure PIC 9(2).
      02 Wminute PIC 9(2).
      02 Wseconde PIC 9(2).
      02 Wmillisecond PIC 9(2).
    77 Wseanceok PIC 9(2).
    77 Widfilmok PIC 9(2).
    77 WidSalleok PIC 9(2).
    77 WidSeanceok PIC 9(2).
    77 Wheureavant PIC 9(2).
    77 Wheureapres PIC 9(2).
    77 WfinSeance PIC 9(2).
    77 reponse PIC 9(2).

    *> variable de la fonction recherche_seances
    77 Wchoixcritere PIC 9(2).
    01 Wdaterecherche.
      02 Wjourrecherche PIC 9(2).
      02 Wmoisrecherche PIC 9(2).
      02 Wanneerecherche PIC 9(4).
    77 Wtitrerecherche PIC A(50).
    77 Wfinseancerecherche PIC 9(2).
    77 Wfinfilmrecherche PIC 9(2).
    77 Wdaterechercheok PIC 9(2).
    77 Wgenrerecherche PIC A(20).
    77 Widfilmrecherche PIC 9(2).

    *> variable de la fonction suppression_seance 
 		77 Wchoixsuppr PIC 9(2).
    77 WidSeance PIC 9(2).
    77 Wfinsupprreserv PIC 9(2).


	*> variable de la fonction montant_journalier
	77 WsommeI PIC 9(4).
	77 WsommeS PIC 9(4).
	77 WsommeP PIC 9(9).
	77 WfinR PIC 9.
	
	*> variable de la fonction affiche_statistique
	77 WfinF PIC 9.
	77 WsommeE PIC 9(9).
	77 Wi PIC 9(9).
	77 Wcompt PIC 9(3).
	01 Wtab.
         02 Wcellule OCCURS 999.
			03 WtitlefilmT PIC A(50).
			03 WnbplaceT PIC 9(9).
	
    *> variable Aldvine
    77 Wchoix PIC 9(2).
    77 Wfin PIC 9(1).
    77 Wcpt PIC 9(5).
    77 Wdate PIC 9(8).
    77 Wtampon PIC 9(8).
    *> date en entier ( ressemble aux timestamp)
    77 WdateInteger PIC 9(8).
    77 WplaceRestante PIC 9(4).
    77 Wplace_enfant PIC 9(4).
    77 Werror PIC 9(1).


    *> variables Andy
    77 Wtrouve PIC 9(2).

    
PROCEDURE DIVISION.
    
    OPEN INPUT fsalles
    IF fsal_stat = 35 THEN
        OPEN OUTPUT fsalles
    END-IF
    CLOSE fsalles
    
    OPEN INPUT fseances
    IF fsea_stat = 35 THEN
        OPEN OUTPUT fseances
    END-IF
    CLOSE fseances
    
    OPEN INPUT ffilms
    IF ff_stat = 35 THEN
        OPEN OUTPUT ffilms
    END-IF
    CLOSE ffilms
    
    OPEN INPUT freservation
    IF fr_stat = 35 THEN
        OPEN OUTPUT freservation
    END-IF
    CLOSE freservation
    
    OPEN INPUT fclients
    IF fc_stat = 35 THEN
        OPEN OUTPUT fclients
    END-IF
    CLOSE fclients
    
    MOVE 0 TO Wmenu
    PERFORM WITH TEST AFTER UNTIL Wmenu=16
        DISPLAY "Que voulez vous faire ?"
        DISPLAY "1-Ajouter séance"
        DISPLAY "2-Recherche séance"
        DISPLAY "3-Suppression séance"
        DISPLAY "4-Ajouter salle"
        DISPLAY "5-Recherche salle"
        DISPLAY "6-Ajout film"
        DISPLAY "7-Recherche film"
        DISPLAY "8-Ajout client"
        DISPLAY "9-Recherche client"
        DISPLAY "10-liste des clients"
        DISPLAY "11-Ajout réservation"
        DISPLAY "12-Recherche réservation"
        DISPLAY "13-Affiche réservations en cours"
        DISPLAY "14-Bénéfice journalier"
        DISPLAY "15-Classement entrée"
        DISPLAY "16-Quitter"
        ACCEPT Wmenu
        EVALUATE Wmenu
        WHEN 1
            PERFORM AJOUT_SEANCE
        WHEN 2
            PERFORM RECHERCHE_SEANCE
        WHEN 3
            PERFORM SUPPRESSION_SEANCE
        WHEN 4
            PERFORM AJOUT_SALLE
        WHEN 5
            PERFORM RECHERCHE_SALLE
        WHEN 6
            PERFORM AJOUT_FILM
        WHEN 7
            PERFORM RECHERCHE_FILM
        WHEN 8
            PERFORM AJOUT_CLIENT
        WHEN 9
            PERFORM RECHERCHE_CLIENT
        WHEN 10
            PERFORM LISTE_CLIENT
        WHEN 11
            PERFORM AJOUT_RESERVATION
        WHEN 12
            PERFORM RECHERCHE_RESERVATION
        WHEN 13
            PERFORM AFFICHE_RESERVATIONS
        WHEN 14
            PERFORM MONTANT_JOURNALIER
        WHEN 15
            PERFORM AFFICHE_STATISTIQUE
        END-EVALUATE
    END-PERFORM
    STOP RUN.
    
    AJOUT_SEANCE.
      MOVE FUNCTION CURRENT-DATE to WdateActu
      PERFORM WITH TEST AFTER UNTIL Wseanceok = 1
        MOVE 0 TO Wanneeok
        PERFORM WITH TEST AFTER UNTIL Wanneeok = 1   
			PERFORM WITH TEST AFTER UNTIL WanneS >= Wannee
				DISPLAY "Veuillez saisir l'année"
				ACCEPT WanneS
			END-PERFORM
			PERFORM WITH TEST AFTER UNTIL WmoisS < 13 AND > 0
				DISPLAY "Veuillez saisir le mois"
				ACCEPT WmoisS
				IF WanneS = Wannee THEN
					IF WmoisS < Wmois THEN
						MOVE 13 TO WmoisS
						DISPLAY "Ce mois est déja passé"
					END-IF
				END-IF
			END-PERFORM
			PERFORM WITH TEST AFTER UNTIL Wjours > 0 AND < 32
				DISPLAY "Veuillez saisir le jour"
				ACCEPT WjourS
				IF Wannes = Wannee THEN
					IF WmoisS = Wmois THEN
						IF WjourS < Wjour THEN
							MOVE 32 TO WjourS
							DISPLAY "Ce jour est déja passé"
						END-IF
					END-IF
				END-IF
			END-PERFORM 
			MOVE FUNCTION CONCATENATE(Wannes,WmoisS,WjourS) TO Wdate
        	IF FUNCTION TEST-DATE-YYYYMMDD(Wdate) = 00000000 THEN
        		MOVE 1 TO Wanneeok
        	ELSE
        		DISPLAY "La date saisie n'est pas correcte"
        	END-IF
    END-PERFORM
		PERFORM WITH TEST AFTER UNTIL WheureS < 23 AND > 9
			DISPLAY "Veuillez saisir l'heure de début de la séance"
			ACCEPT WheureS
			IF Wannes = Wannee THEN
				IF WmoisS = Wmois THEN
					IF WjourS = Wjour THEN
						IF WheureS < Wheure THEN
							MOVE 24 TO WheureS
							DISPLAY "Cette heure est déja passée"
						END-IF
					END-IF
				END-IF
			END-IF
		END-PERFORM
		PERFORM WITH TEST AFTER UNTIL WminuteS < 60 AND > -1
			DISPLAY "Veuillez saisir la minute à laquelle commence la séance"
			ACCEPT WminuteS
			IF Wannes = Wannee THEN
				IF WmoisS = Wmois THEN
					IF WjourS = Wjour THEN
						IF WheureS = Wheure THEN
							IF WminuteS < Wminute THEN
								MOVE 60 TO WminuteS
								DISPLAY "Cet horaire est déja passé"
							END-IF
						END-IF
					END-IF
				END-IF
			END-IF
		END-PERFORM
    MOVE 0 TO Wseanceok
    MOVE 0 TO Widfilmok
    MOVE 0 TO WidSalleok
    MOVE 0 TO WidSeanceok
    MOVE 1 TO reponse
      OPEN INPUT ffilms
      PERFORM WITH TEST AFTER UNTIL Widfilmok = 1
        DISPLAY "Veuillez saisir l'id du film"
        ACCEPT WidfilmS
        MOVE WidfilmS TO ff_id
        START ffilms 
          INVALID KEY
            DISPLAY "Ce film n'existe pas"
            DISPLAY "Voulez vous continuez ? 0 pour non 1 pour oui"
            ACCEPT reponse
            IF reponse = 0 THEN
              MOVE 1 TO Widfilmok
              MOVE 1 TO Wseanceok
            END-IF
          NOT INVALID KEY
            MOVE 1 TO Widfilmok
        END-START
      END-PERFORM
      CLOSE ffilms
      IF reponse = 1 THEN
        OPEN INPUT fsalles
          PERFORM WITH TEST AFTER UNTIL WidSalleok = 1
            DISPLAY "Veuillez saisir l'id de la salle"
            ACCEPT WnumsalleS
            MOVE WnumsalleS TO fsal_num
            START fsalles
              INVALID KEY
                DISPLAY "Cette salle n'existe pas"
                DISPLAY "Voulez vous continuez ? 0 pour non 1 pour oui"
                ACCEPT reponse
                IF reponse = 0 THEN
                  MOVE 1 TO WidSalleok
                  MOVE 1 TO Wseanceok
                END-IF
              NOT INVALID KEY
                MOVE 1 TO WidSalleok
            END-START
          END-PERFORM
        CLOSE fsalles
        IF reponse = 1 THEN
          PERFORM WITH TEST AFTER UNTIL WtypedifS = 0 OR WtypedifS = 1
            DISPLAY "Veuillez saisir si la séance est de type 3D (0 pour non 1 pour oui)"
            ACCEPT WtypedifS
          END-PERFORM
          OPEN I-O fseances
          PERFORM WITH TEST AFTER UNTIL WidSeanceok = 1
            DISPLAY "Veuillez saisir l'id de la séance"
            ACCEPT WidS
            MOVE WidS TO fsea_id
            START fseances 
              INVALID KEY
                MOVE 1 TO WidSeanceok
              NOT INVALID KEY
                DISPLAY "Ce numéro de séance est déja utilisé"
            END-START
          END-PERFORM
          MOVE WnumsalleS TO fsea_numsalle
          MOVE WjourS TO fsea_jour
          MOVE WmoisS TO fsea_mois
          MOVE WanneS TO fsea_annee
          MOVE FUNCTION CONCATENATE(Wheure,Wminute) TO fsea_horaire
          MOVE 0 TO WfinSeance
          MOVE 1 TO Wseanceok
          DISPLAY fsea_date
          START fseances KEY = fsea_date
            INVALID KEY
              MOVE 1 TO WfinSeance
              MOVE 1 TO Wseanceok
            NOT INVALID KEY
              IF WnumsalleS = fsea_numsalle THEN
                COMPUTE Wheureavant = WheureS - fsea_heure
                IF Wheureavant < 3 AND >= 0 THEN
                  DISPLAY "Il y a déja une séance prévu dans ce créneau horaire"
                  MOVE 1 TO WfinSeance
                  MOVE 0 TO Wseanceok
                ELSE
                  COMPUTE Wheureapres = Wheureapres - WheureS
                  IF Wheureapres < 3 AND >= 0 THEN
                    DISPLAY "Il y a déja une séance prévu dans ce créneau horaire"
                    MOVE 1 TO WfinSeance
                    MOVE 0 TO Wseanceok
                  END-IF
                END-IF
              END-IF
            PERFORM WITH TEST AFTER UNTIL WfinSeance = 1
              READ fseances NEXT
                AT END 
                  MOVE 1 TO WfinSeance
                NOT AT END 
                  IF WnumsalleS = fsea_numsalle THEN
                    COMPUTE Wheureavant = WheureS - fsea_heure
                    IF Wheureavant < 3 AND >= 0 THEN
                      DISPLAY "Il y a déja une séance prévu dans ce créneau horaire"
                      MOVE 1 TO WfinSeance
                      MOVE 0 TO Wseanceok 
                    ELSE
                      COMPUTE Wheureapres = Wheureapres - WheureS
                      IF Wheureapres < 3 AND >= 0 THEN
                        DISPLAY "Il y a déja une séance prévu dans ce créneau horaire"
                        MOVE 1 TO WfinSeance
                        MOVE 0 TO Wseanceok 
                      END-IF
                    END-IF
                  END-IF
              END-READ
            END-PERFORM
          END-START
        END-PERFORM
        IF reponse = 1 THEN
          MOVE WidS  TO fsea_id
          MOVE WjourS TO fsea_jour
          MOVE WmoisS TO fsea_mois
          MOVE WanneS TO fsea_annee
          MOVE WminuteS TO fsea_minute
          MOVE WheureS TO fsea_heure
          MOVE WnumsalleS TO fsea_numsalle
          MOVE WidfilmS TO fsea_idfilm
          MOVE WtypedifS TO fsea_typedif
          WRITE seaTampon
          END-WRITE
          CLOSE fseances
          IF fsea_stat = 00 THEN
            DISPLAY "Seance ajoutée"
          ELSE 
            DISPLAY "erreur enregistrement", fsea_stat
          END-IF
        END-IF.

    RECHERCHE_SEANCE.

      PERFORM WITH TEST AFTER UNTIL Wchoixcritere = 1 OR = 2
        DISPLAY "Par quel critère voulez vous chercher les séances : 1 par date, 2 par genre"
        ACCEPT Wchoixcritere
      END-PERFORM
      MOVE FUNCTION CURRENT-DATE to WdateActu
      IF Wchoixcritere = 1 THEN
        MOVE 0 TO Wdaterechercheok
        PERFORM WITH TEST AFTER UNTIL Wdaterechercheok = 1
          DISPLAY "Veuillez saisir la date au format ddmmyyyy"
          ACCEPT Wdaterecherche
          MOVE FUNCTION CONCATENATE(Wanneerecherche,Wmoisrecherche,Wjourrecherche) TO Wdate
          IF FUNCTION TEST-DATE-YYYYMMDD(Wdate) = 00000000 THEN
            MOVE 1 TO Wdaterechercheok
          ELSE
            DISPLAY "La date saisie n'existe pas"
          END-IF
        END-PERFORM
        MOVE Wdaterecherche TO fsea_date
        OPEN INPUT fseances
        START fseances KEY = fsea_date
          INVALID KEY
            DISPLAY "Pas de séance existante à cette date"
            MOVE 1 TO Wfinseancerecherche
          NOT INVALID KEY
          PERFORM WITH TEST AFTER UNTIL Wfinseancerecherche = 1
            READ fseances NEXT
              AT END 
                MOVE 1 TO Wfinseancerecherche
              NOT AT END 
                MOVE fsea_idfilm TO ff_id
                OPEN INPUT ffilms
                READ ffilms
                INVALID KEY
                  DISPLAY "Erreur lors de la recherche du film de la séance, celui-ci n'existe pas"
                NOT INVALID KEY
                  MOVE ff_titre TO Wtitrerecherche
                DISPLAY "La séance numéro ",fsea_id," se déroule dans la salle ",fsea_numsalle," avec le film ",Wtitrerecherche
                CLOSE ffilms
            END-READ
          END-PERFORM
        END-START
        CLOSE fseances
      ELSE
        DISPLAY "Veuillez saisir le genre dont vous voulez voir toutes les séances"
        ACCEPT Wgenrerecherche
        OPEN INPUT ffilms
        OPEN INPUT fseances
        MOVE Wgenrerecherche TO ff_genre
        START ffilms KEY = ff_genre
          INVALID KEY
            MOVE 1 TO Wfinfilmrecherche
            DISPLAY "Aucun film ne correspond à votre critère de recherche"
          NOT INVALID KEY
          PERFORM WITH TEST AFTER UNTIL Wfinfilmrecherche = 1
            READ ffilms NEXT
              AT END 
                MOVE 1 TO Wfinfilmrecherche
              NOT AT END 
                MOVE ff_id TO Widfilmrecherche
                MOVE ff_titre TO Wtitrerecherche
                START fseances KEY = fsea_idfilm
                INVALID KEY
                  DISPLAY "Erreur lors de la recherche du film dans la séance, celui-ci n'existe pas"
                PERFORM WITH TEST AFTER UNTIL Wfinseancerecherche = 1
                  READ fseances NEXT
                    AT END 
                      MOVE 1 TO Wfinseancerecherche
                    NOT AT END
                      DISPLAY "La séance numéro ",fsea_id," se déroule dans la salle ",fsea_numsalle," avec le film ",Wtitrerecherche
                  END-READ
                END-PERFORM
                END-START
            END-READ
          END-PERFORM
        END-START
        CLOSE fseances
        CLOSE ffilms
      END-IF.

    SUPPRESSION_SEANCE.
      MOVE 0 TO Wchoixsuppr
        DISPLAY "Veuillez saisir l'id de la seance à supprimer"
        ACCEPT WidSeance
        OPEN I-O fseances
        MOVE WidSeance TO fsea_id
        READ fseances
          INVALID KEY 
            DISPLAY "Il n'existe pas de séance possédant ce numéro"
          NOT INVALID KEY
            OPEN I-O freservation
            MOVE WidSeance TO fr_idseance
            START freservation KEY = fr_idseance
            INVALID KEY 
              DISPLAY "Cette séance ne possède pas de réservation celle-ci va être supprimer"
            NOT INVALID KEY
              DISPLAY "Cette séance possède des réservations voulez vous quand même la supprimer : 0 pour non 1 pour oui "
              ACCEPT Wchoixsuppr
            IF Wchoixsuppr = 1
              READ freservation NEXT
              AT END
                MOVE 1 TO Wfinsupprreserv
              NOT AT END
                IF fr_idseance = WidSeance THEN
                  DELETE freservation RECORD
                ELSE
                  MOVE 1 TO Wfinsupprreserv
                END-IF
              END-READ
            END-IF
            END-START
            CLOSE freservation
          IF Wchoixsuppr = 0
            DELETE fseances RECORD
        END-READ
        CLOSE fseances
        IF fsea_stat = 00 THEN
          DISPLAY "Suppression séance"
        ELSE
          DISPLAY "Erreur suppression séance"
        END-IF.
    
    AJOUT_SALLE.
    
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
              PERFORM WITH TEST AFTER UNTIL WnumS <> ' '
                     DISPLAY 'Quel est le numéro de la salle à ajouter ?'			
                     ACCEPT WnumS
              end-perform
              PERFORM WITH TEST AFTER UNTIL WnbplaceS <> ' '
                     DISPLAY 'Quel est le nombre de places de la salle ?'
                     ACCEPT WnbplaceS
              END-PERFORM
              
              OPEN INPUT fsalles
              MOVE 0 TO Wtrouve
              MOVE 0 TO Wfin
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouve = 1
                     READ fsalles NEXT
                     AT END MOVE 1 TO Wfin
                     NOT AT END 
                            IF WnumS = fsal_num THEN
                                   MOVE 1 TO Wtrouve
                            END-IF
                     END-READ
              END-PERFORM
              CLOSE fsalles
       END-PERFORM
       MOVE WnumS TO fsal_num
       MOVE WnbplaceS TO fsal_nbplace
       OPEN I-O fsalles
	WRITE salTampon
	END-WRITE
	CLOSE fsalles.
       
    RECHERCHE_SALLE.
    
       OPEN INPUT fsalles
       MOVE 0 TO Wfin
       DISPLAY 'Quel est le numéro de la salle ?'
       ACCEPT WnumS
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR Wfin = 1
              READ fsalles NEXT
              AT END MOVE 1 TO Wfin
                     DISPLAY 'Salle inexistante'
              NOT AT END
                     IF fsal_num = WnumS THEN
                            MOVE 1 TO Wtrouve
                            DISPLAY 'Numéro de la salle :', fsal_num
                            DISPLAY 'Nombre de places de la salle :', fsal_nbplace
                     END-IF
              END-READ
       END-PERFORM
       CLOSE fsalles.
    
    AJOUT_FILM.
		MOVE 0 TO Wtrouve
		PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
			PERFORM WITH TEST AFTER UNTIL WidF <> ' '
				DISPLAY 'Quel est le numéro du film à ajouter ?'
				ACCEPT WidF
			END-PERFORM
			OPEN I-O ffilms
			MOVE WidF TO ff_id
			START ffilms key = ff_id
			INVALID KEY
				MOVE 1 TO Wtrouve
				PERFORM WITH TEST AFTER UNTIL ff_titre <> ' '
                     DISPLAY 'Quel est le titre du film ?'
                     ACCEPT ff_titre
				END-PERFORM
				PERFORM WITH TEST AFTER UNTIL ff_genre <> ' '
                     DISPLAY 'Quel est le genre du film ?'
                     ACCEPT ff_genre
				END-PERFORM
				PERFORM WITH TEST AFTER UNTIL ff_annee <> ' '
                     DISPLAY 'En quel année est sorti le film ?'
                     ACCEPT ff_annee
				END-PERFORM
			NOT INVALID KEY
				DISPLAY "id du film déjà pris"
            END-START
       END-PERFORM
	   WRITE filmTampon END-WRITE
	   IF ff_stat <> 0 THEN
			DISPLAY "Erreur enregistrement : ",ff_stat
	   END-IF
	   CLOSE ffilms.
    
    RECHERCHE_FILM.
		OPEN INPUT ffilms
		DISPLAY "Saisir le genre du film recherché"
		ACCEPT WgenreF
		DISPLAY "--------------------------------"
		MOVE 0 TO WfinF
		MOVE 1 TO Wcpt
		PERFORM WITH TEST AFTER UNTIL WfinF = 1
			READ ffilms NEXT
			AT END
				MOVE 1 TO WfinF
			NOT AT END
				IF ff_genre = WgenreF THEN
					MOVE 0 TO Wcpt
					DISPLAY "id film : ", ff_id
					DISPLAY "titre : ", ff_titre
					DISPLAY "genre : ", ff_genre
					DISPLAY "année : ", ff_annee
					DISPLAY "--------------------------------"
				END-IF
			END-READ
		END-PERFORM
		IF Wcpt = 1 THEN
			DISPLAY "Aucun film trouvé"
			DISPLAY "--------------------------------"
		END-IF
		CLOSE ffilms.
    
    AJOUT_CLIENT.
    
        OPEN I-O fclients
        DISPLAY "Veuillez saisir l'adresse mail du client"
        ACCEPT fc_mail
        READ fclients
        INVALID KEY 
           
            DISPLAY "Veuillez saisir le prenom du client"
            ACCEPT fc_prenom
            DISPLAY "Voulez-vous ajouter un abonnement ?"
            Display " 0-NON"
            Display " 1-OUI"
            ACCEPT Wchoix
            IF Wchoix=1 THEN
            MOVE 1 to Wfin
             *> ajout de la date courante dans la variable
                   MOVE FUNCTION CURRENT-DATE to fc_datedeb
                 
                PERFORM WITH TEST AFTER UNTIL fc_duree >=1  
                 DISPLAY "Saisir la durée de l'abonnement (en mois >1)"
                 ACCEPT fc_duree
               END-PERFORM
            END-IF

            WRITE clieTampon END-WRITE
            IF fc_stat=00 THEN
              DISPLAY "Enregistrement reussi"
            ELSE 
              DISPLAY "Echec Enregistrement"
            END-IF


        NOT INVALID KEY 
           Display "Cette adresse e-mail existe déjà souhaitez vous mettre à jour cette fiche ?"
           Display " 0-NON"
           Display " 1-OUI"
           ACCEPT Wchoix
           IF Wchoix=1 THEN
                 DISPLAY "Veuillez saisir le prenom du client"
               ACCEPT fc_prenom
               DISPLAY "Voulez-vous mettre à jour l'abonnement ?"
               Display " 0-NON"
               Display " 1-OUI"
               ACCEPT Wchoix
               IF Wchoix=1 THEN
              *> utile pour tester la date
             *>   MOVE FUNCTION CONCATENATE(fc_annee,fc_mois,fc_jour) TO Wdate
                *>IF FUNCTION TEST-DATE-YYYYMMDD(Wdate) = 00000001 THEN
                
                  *> ajout de la date courante dans la variable s'il n'y a pas de date
                  MOVE FUNCTION CURRENT-DATE to fc_datedeb           
               
                   PERFORM WITH TEST AFTER UNTIL fc_duree >=1  
                    DISPLAY "Saisir la durée de l'abonnement (en mois >1)"
                    ACCEPT fc_duree
                  END-PERFORM
               END-IF
               REWRITE clieTampon
               IF fc_stat=00 THEN
                 DISPLAY "Enregistrement reussi"
               ELSE 
                 DISPLAY "Echec Enregistrement"
               END-IF
           END-IF
        END-READ
       
        CLOSE fclients.
    
    RECHERCHE_CLIENT.
    
      OPEN INPUT fclients
      DISPLAY "saisir l'adresse mail du client"
      ACCEPT fc_mail
      READ fclients
      INVALID KEY 
       DISPLAY "Aucun client ne correspond à cette adresse"
       display "--------------------------------"
      NOT INVALID KEY
      display "--------------------------------"
       DISPLAY "CLIENT : ", fc_mail
       DISPLAY "Prenom : ", fc_prenom
       DISPLAY "date de debut d'abonnement : ", fc_jour,"/",fc_mois,"/",fc_annee
       DISPLAY "Durée de l'abonnement (en mois) :",fc_duree
       display "--------------------------------"
      END-READ
      CLOSE fclients.

    LISTE_CLIENT.
    
      
      PERFORM WITH TEST AFTER UNTIL Wchoix =1 OR Wchoix =2
        DISPLAY "Que souhaitez-vous faire ? "
        DISPLAY "1 -Afficher tout les clients "
        DISPLAY "2 - Afficher les client avec le choix du prenom"
        ACCEPT Wchoix
      END-PERFORM
    
      OPEN INPUT fclients
      IF Wchoix = 1 THEN
       MOVE 0 to Wfin
       MOVE 0 to Wcpt
       PERFORM WITH TEST AFTER UNTIL Wfin =1 
           COMPUTE Wcpt = Wcpt + 1
           READ fclients NEXT
           AT END
              MOVE 1 to Wfin
           NOT AT END
              DISPLAY "-----------------------------"
              DISPLAY "Client ",Wcpt
              
              DISPLAY "Email : ", fc_mail
              DISPLAY "Prenom: ", fc_mail
              DISPLAY "date de debut d'abonnement : ", fc_jour,"/",fc_mois,"/",fc_annee
              DISPLAY "Durée de l'abonnement (en mois) :",fc_duree
              DISPLAY "-----------------------------"


           END-READ   
       END-PERFORM
      ELSE

        IF Wchoix= 2 THEN
         DISPLAY 'Saisir le prenom du client'
         ACCEPT WprenomC
         MOVE 0 to Wfin
         MOVE 0 to Wcpt
         PERFORM WITH TEST AFTER UNTIL Wfin =1 
              READ fclients NEXT
              AT END
                 MOVE 1 to Wfin
              NOT AT END
                 IF WprenomC = fc_prenom THEN
                 
                     COMPUTE Wcpt = Wcpt + 1
                     DISPLAY "-----------------------------"
                     DISPLAY "Client ",Wcpt
                     
                     DISPLAY "Email : ", fc_mail
                     DISPLAY "Prenom: ", fc_mail
                     DISPLAY "date de debut d'abonnement : ", fc_jour,"/",fc_mois,"/",fc_annee
                     DISPLAY "Durée de l'abonnement (en mois) :",fc_duree
                     DISPLAY "-----------------------------"              

                 END-IF
              END-READ   
         END-PERFORM
        END-IF
      END-IF
      IF Wcpt = 0 THEN
       DISPLAY "Aucun client à afficher"
      END-IF

      
      CLOSE fclients.
    
    AJOUT_RESERVATION.
        DISPLAY "--------------ajout reservation--------------"
        *> fonction qui retourne le nombre de jour depuis le 1600/12/31
        

           *> demande de la seance
        *> saisir le numero de la reservation
        *> verifiacation si existe deja
        
        *> demande du nombre de place normal
        *> demande du nombre de place enfant
        *> verification des places restantes 
        *> demandes des id des abonnées et verif de leur abonenment
        *> calcul du montant total
        *> ecriture
        *> affichage recap commande
        OPEN INPUT fseances
        
        DISPLAY "Saisir le numéro de la séance : "
        ACCEPT fsea_id
        READ fseances
           INVALID KEY 
               DISPLAY "Aucune séance ne porte ce numéro"
           NOT INVALID KEY 
              OPEN I-O freservation
              MOVE fsea_id to WidseanceR
              MOVE 0 to Wfin
               PERFORM WITH TEST AFTER UNTIL Wfin =1 
                 DISPLAY "Saisir le numéro de la réservation : "
                  ACCEPT WnumR
                  MOVE WnumR to fr_num
                   READ freservation
                    INVALID KEY 
                       MOVE 1 to Wfin
                    NOT INVALID KEY 
                       DISPLAY "Numéro de reservation déjà existant, saisissez en un nouveau"
                    END-READ
               END-PERFORM
               OPEN INPUT fsalles
               DISPLAY "Saisir le nombre de places à commander"
               ACCEPT WplaceR
                 MOVE WidseanceR TO fr_idseance
                  *> se positionner
                MOVE 0 to Werror
                MOVE 0 TO WplaceRestante
                START freservation key = fr_idseance
                invalid key
                    MOVE fsea_numsalle TO fsal_num
                    READ fsalles
                        INVALID KEY 
                           MOVE 1 to Werror
                          DISPLAY "Erreur , la seance n'as pas de salle"
                        NOT INVALID KEY 
                          COMPUTE WplaceRestante = fsal_nbplace 
                    END-READ
                not invalid key
                    MOVE 0 TO Wfin
                        *> lecture sur zone indexe 
                     MOVE 0 to WnbplaceS
                     PERFORM WITH TEST AFTER UNTIL Wfin =1
                        READ freservation NEXT
                        AT END
                            MOVE 1 TO Wfin
                        NOT AT END
                            COMPUTE WnbplaceS = WnbplaceS + fr_place
                        END-READ
                     END-PERFORM
                     
                     MOVE fsea_numsalle TO fsal_num
                     READ fsalles
                        INVALID KEY 
                          MOVE 1 to Werror
                          DISPLAY "Erreur , la seance n'as pas de salle"
                        NOT INVALID KEY 
                          COMPUTE WplaceRestante = fsal_nbplace - WnbplaceS
                    END-READ
                END-START 
                 CLOSE fsalles  
                  MOVE 0 TO WmontantR
                IF WplaceR <=  WplaceRestante AND Werror <> 1 THEN
                        PERFORM WITH TEST AFTER UNTIL Wplace_enfant <= WplaceR AND Wplace_enfant >=0
                          MOVE 0 TO WmontantR
                         DISPLAY "Saisir le nombre de places enfant "
                         ACCEPT Wplace_enfant
                         COMPUTE WmontantR = WmontantR + WtarifEnfant * Wplace_enfant
                         display "montant enfant  : ",WmontantR
                        END-PERFORM
                        COMPUTE Wtampon = WplaceR - Wplace_enfant
                        IF Wtampon >0 THEN
                           PERFORM WITH TEST AFTER UNTIL Wplace_abonneR <= Wtampon
                            DISPLAY "Saisir le nombre de places abonnés inferieur ou egale à ",Wtampon
                            ACCEPT Wplace_abonneR
                           END-PERFORM
                       ELSE
                          MOVE 0 to Wplace_abonneR
                       END-IF
                       COMPUTE  Wtampon = Wtampon - Wplace_abonneR
                        *> ajout du tarif 3D ou non
                        COMPUTE WmontantR = WmontantR + Wtampon* WtarifAdulte
                        IF fsea_typedif =1 THEN
                          COMPUTE WmontantR = WmontantR + WplaceR * Wtarif3D
                        END-IF
                        MOVE 0 to Wcpt
                        OPEN INPUT fclients
                        IF Wcpt <> Wplace_abonneR THEN
                         PERFORM WITH TEST AFTER UNTIL Wcpt >= Wplace_abonneR 
                             COMPUTE Wcpt = Wcpt + 1
                             DISPLAY "saisir le mail de l'abonné no ",Wcpt
                             ACCEPT fc_mail
                             READ fclients
                                INVALID KEY 
                                   DISPLAY "Abonné non existant"
                                   DISPLAY "Tarif normal adulte s'applique"
                                   COMPUTE WmontantR = WmontantR + WtarifAdulte
                                 
                                NOT INVALID KEY
                                   *> verification de sa date d'abonnement
                                   MOVE FUNCTION INTEGER-OF-DATE(fc_datedeb) to WdateInteger
                                   *> ajout de 30 jour x le nombre de mois
                                   COMPUTE WdateInteger = WdateInteger + fc_duree * 30
                                   MOVE FUNCTION INTEGER-OF-DATE( FUNCTION CURRENT-DATE) TO Wdate
                                   IF Wdate< WdateInteger THEN
                                   *> abonnement encore valide
                                      COMPUTE WmontantR = WmontantR + WtarifReduc
                                   ELSE
                                      DISPLAY "L'abonnement de ",fc_prenom," a expiré le tarif normal adulte s'applique"
                                      COMPUTE WmontantR = WmontantR + WtarifAdulte
                                   END-IF
                            END-READ
                          END-PERFORM
                         END-IF
                         CLOSE fclients
                          MOVE WmontantR to fr_montant
                          MOVE WplaceR to fr_place
                          MOVE Wplace_abonneR to fr_placeAbonne
                          MOVE WidseanceR to fr_idseance
                          MOVE WnumR to fr_num
      
                         WRITE reserTampon
                         END-WRITE
                         IF fc_stat = 00 THEN
                          DISPLAY "--------RECAPITULATIF RESERVATION ---------"
                          DISPLAY " Seance no ",fsea_id
                          DISPLAY "DATE : ",fsea_date
                          DISPLAY "HEURE : ",fsea_horaire
                          DISPLAY "nombre de place reserver : ",fr_place
                          DISPLAY "dont enfant : ",Wplace_enfant
                          DISPLAY "montant total à payer : ",fr_montant
                         ELSE
                            DISPLAY "Erreur inconnue, Impossible d'enregistrer cette reservation"
                         END-IF
                      ELSE
                       DISPLAY "ERREUR, il ne reste que ",WplaceRestante," places pour cette seance"
                       DISPLAY "et vous en demandez ",WplaceR
                      END-IF        
              CLOSE freservation
        END-READ
       
        CLOSE fseances
        DISPLAY "---------------fin ajout reservation---------------".
      
    RECHERCHE_RESERVATION.
      OPEN INPUT freservation
    
        DISPLAY "----------------- DEBUT recherche réservation ---------------"
           DISPLAY "Saisir le numéro de la reservation"
           ACCEPT fr_num
           READ freservation
              INVALID KEY
                 DISPLAY " Aucune reservation pour ce numéro "
              NOT INVALID KEY
                  DISPLAY "--------RECAPITULATIF RESERVATION ---------"
                   DISPLAY " Reservation no ",fr_num
                   DISPLAY " Seance no ",fr_idseance
                   DISPLAY "nombre de place reserver : ",fr_place
                   DISPLAY "dont abonne : ",fr_placeAbonne
                   DISPLAY "montant total à payer : ",fr_montant
           END-READ
       CLOSE freservation
        DISPLAY "----------------- FIN recherche réservation ---------------".
    
    AFFICHE_RESERVATIONS.
       *> parcours squentiel du fichier seance en premier moins gourmand
        DISPLAY "----------------- DEBUT affiche Reservations ---------------"
        DISPLAY "----Affiche les reservations des seances du jours et celles à venir ----"
        OPEN INPUT fseances
        OPEN INPUT freservation
        MOVE 0 TO Wfin
        MOVE 0 TO Wcpt
        PERFORM WITH TEST AFTER UNTIL Wfin =1
            READ fseances NEXT
                AT END 
                 MOVE 1 TO Wfin
                NOT AT END 
                 COMPUTE Wcpt = Wcpt + 1
                 *> verif de la date
                  MOVE FUNCTION INTEGER-OF-DATE(fsea_date) to Wdate
                  MOVE FUNCTION INTEGER-OF-DATE( FUNCTION CURRENT-DATE) TO WdateInteger
                  IF Wdate>= WdateInteger THEN
                    MOVE fsea_id to fr_idseance
                      START freservation key = fr_idseance
                          INVALID KEY
                             DISPLAY " "
                          NOT INVALID KEY
                          *> affichage de la seance 
                          DISPLAY "--- --- --- ---Seance ",fsea_id,"-- --- --- ---"
                          DISPLAY "DATE : ",fsea_date
                          DISPLAY "HEURE : ",fsea_horaire
                          MOVE 0 TO Wfin
                          PERFORM WITH TEST AFTER UNTIL Wfin=1
                              READ freservation NEXT
                                AT END
                                   MOVE 1 TO Wfin
                                NOT AT END
                                *> affichage de toutes les reservation
                                 DISPLAY "--- RESERVATION ",fr_num,"---"
                                 DISPLAY "nombre de place reserver : ",fr_place
                                 DISPLAY "dont abonne : ",fr_placeAbonne
                                 DISPLAY "montant total à payer : ", fr_montant
                             END-READ
                          END-PERFORM
                      END-START
                  END-IF
            END-READ
        END-PERFORM
        CLOSE freservation
        CLOSE fseances
        IF Wcpt = 0 THEN
        DISPLAY "Aucune Reservation pour les seances du jour et celles à venir"
        END-IF
        DISPLAY "----------------- FIN affiche Reservations ---------------".
    
    MONTANT_JOURNALIER.
        DISPLAY "Saisir la date du jour sous le format JJMMYYYY"
        ACCEPT fsea_date
        MOVE fsea_jour TO WjourS
        MOVE fsea_mois TO WmoisS
        MOVE fsea_annee TO WanneS
        OPEN INPUT fseances
        OPEN INPUT freservation
        START fseances key = fsea_date
        INVALID KEY
			DISPLAY "Aucune séance pour cette date"
		NOT INVALID KEY
			DISPLAY " "
			DISPLAY "Voici la liste des séances :"
			MOVE 0 TO Wfin
			PERFORM WITH TEST AFTER UNTIL Wfin=1
				READ fseances NEXT
				AT END
					MOVE 1 TO Wfin
				NOT AT END
					IF WjourS<>fsea_jour OR WmoisS<>fsea_mois OR WanneS<>fsea_annee THEN
						MOVE 1 TO Wfin
					ELSE
						MOVE fsea_id TO fr_idseance
						START freservation key = fr_idseance
						INVALID KEY
							DISPLAY " "
						NOT INVALID KEY
							MOVE 0 TO WfinR
							MOVE 0 TO WsommeI
							PERFORM WITH TEST AFTER UNTIL WfinR=1
								READ freservation NEXT
								AT END
									MOVE 1 TO WfinR
								NOT AT END
									IF fr_idseance<>fsea_id THEN
										MOVE 1 TO WfinR
									ELSE
										COMPUTE WsommeI = WsommeI + fr_montant
									END-IF
								END-READ
							END-PERFORM
						END-START
						DISPLAY "Horaire: ",fsea_heure,":",fsea_minute," Montant: ",WsommeI
					END-IF
					COMPUTE WsommeS = WsommeS + WsommeI
				END-READ
			END-PERFORM
		END-START
		DISPLAY "Chiffre d'affaire de la journée: ",WsommeS
		CLOSE fseances
		CLOSE freservation.
    
    AFFICHE_STATISTIQUE.
		
		INITIALIZE Wtab.
        OPEN INPUT fseances
        OPEN INPUT freservation
        OPEN I-O ffilms
        MOVE 0 TO WfinF
        MOVE 1 TO Wcompt
        PERFORM WITH TEST AFTER UNTIL WfinF = 1
			READ ffilms NEXT
			AT END
				MOVE 1 TO WfinF
			NOT AT END
				MOVE ff_id TO fsea_idfilm
				START fseances key = fsea_idfilm
				INVALID KEY
					MOVE 1 TO Wfin
				NOT INVALID KEY
					MOVE 0 TO Wfin
					PERFORM WITH TEST AFTER UNTIL Wfin=1
						READ fseances NEXT
						AT END
							MOVE 1 TO Wfin
						NOT AT END
							IF ff_id<>fsea_idfilm THEN
								MOVE 1 TO Wfin
							ELSE
								MOVE fsea_id TO fr_idseance
								START freservation key = fr_idseance
								INVALID KEY
									MOVE 0 TO WfinR
								NOT INVALID KEY
									MOVE 0 TO WfinR
									MOVE 0 TO WsommeE
									MOVE 0 TO WsommeP
									PERFORM WITH TEST AFTER UNTIL WfinR=1
										READ freservation NEXT
										AT END
											MOVE 1 TO WfinR
										NOT AT END
											IF fr_idseance<>fsea_id THEN
												MOVE 1 TO WfinR
											ELSE
												COMPUTE WsommeP = WsommeP + fr_place
											END-IF
										END-READ
									END-PERFORM
								END-START
							END-IF
						END-READ
					END-PERFORM
				END-START
				MOVE WsommeP TO WnbplaceT(Wcompt)
				MOVE ff_titre TO WtitlefilmT(Wcompt)
				COMPUTE Wcompt = Wcompt + 1
			END-READ
		END-PERFORM
		
		COMPUTE Wcompt = Wcompt - 1
		SORT WnbplaceT DESCENDING.
		
		DISPLAY "Classement des films par entrée :"
		PERFORM TEST AFTER VARYING Wi FROM 1 BY 1 UNTIL Wi = Wcompt
			DISPLAY Wi," : ",WtitlefilmT(Wi)," avec ",WnbplaceT(Wi)
		END-PERFORM
		
		CLOSE fseances
		CLOSE freservation
		CLOSE ffilms.
