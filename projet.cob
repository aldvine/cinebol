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
    ALTERNATE RECORD KEY fsea_idfilm
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
        02 fsea_id PIC 9(2).
        02 fsea_date.
			03 fsea_jour PIC 9(2).
			03 fsea_mois PIC 9(2).
			03 fsea_annee PIC 9(4).
		02 fsea_horaire.
			03 fsea_minute PIC 9(2).
			03 fsea_heure PIC 9(2).
        02 fsea_numsalle PIC 9(2).
        02 fsea_idfilm PIC 9(2).			
        02 fsea_typedif PIC 9.
        
    FD fsalles.
    01 salTampon.
        02 fsal_num PIC 9(2).
        02 fsal_nbplace PIC 9(3).
        
    FD ffilms.
    01 filmTampon.
        02 ff_id PIC 9(3).
        02 ff_titre PIC A(50).
        02 ff_genre PIC A(20).
        02 ff_annee PIC 9(2).

    FD fclients.
    01 clieTampon.
        02 fc_mail PIC A(500).    
        02 fc_prenom PIC A(30).
        02 fc_datedeb.
           03 fc_jour PIC 9(2).
           03 fc_mois PIC 9(2).
           03 fc_annee PIC 9(4).
        02 fc_duree PIC 9(2).
        
    FD freservation.
    01 reserTampon.
        02 fr_num PIC 9(2).    
        02 fr_idseance PIC 9(2).    
        02 fr_place PIC 9(2).    
        02 fr_montant PIC 9(2).    
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
    77 WtarifAdulte PIC 99V99 VALUE 6.50.
    77 WtarifEnfant PIC 9V99 VALUE 3.50.
    77 WtarifReduc PIC 9 VALUE 5.
    77 Wtarif3D PIC 9V99 VALUE 1.50.
    
    *> variable autre
    77 Wmenu PIC 9(2).
    
    
    
    *> variables du fichier seances.dat
    77 WidS PIC 9(2).
    77 WnumsalleS PIC 9(2).
    77 WidfilmS PIC 9(2).
    77 WtypedifS PIC 9(2).
    77 WminuteS PIC 9(2).
    77 WheureS PIC 9(2).
    77 WjourS PIC 9(2).
    01 WmoisS PIC 9(2).
		88 moispair VALUE 4,6,8,10,12.
		88 moisfevrier VALUE 2.
    77 WanneS PIC 9(4).
    
    *> variables du fichier salles.dat
    77 WnumS PIC 9(2).
    77 WnbplaceS PIC 9(3).
    
    *> variables du fichier films.dat
    77 WidF PIC 9(2).
    77 WtitreF PIC A(50).
    77 WgenreF PIC A(20).
    77 WanneeF PIC 9(2).
    
    *> variables du fichier clients.dat
    77 WmailC PIC A(250).    
    77 WprenomC PIC A(30).
    77 WdatedebC PIC X(15).
    77 WdureeC PIC 9(2).
    
    *> variables du fichier reservations.dat
    77 WnumR PIC 9(2).    
    77 WidseanceR PIC 9(2).    
    77 WplaceR PIC 9(2).    
    77 WmontantR PIC 9(2).
    77 Wplace_abonneR PIC 9(2).
    
    *> variable de la fonction ajout_seances
    77 jourok PIC 9(2).
    77 dateactuel PIC 9(8).
    77 annee PIC 9(4).
	77 anneediv PIC 9(4).
	77 anneedivreste PIC 9(4).

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
		ACCEPT dateactuel FROM DATE YYYYMMDD
		PERFORM WITH TEST AFTER UNTIL Wannes >= annee
			DISPLAY "Veuillez saisir l'année"
			ACCEPT WanneS
		END-PERFORM
		PERFORM WITH TEST AFTER UNTIL WmoisS < 13 AND > 0
			DISPLAY "Veuillez saisir le mois"
			ACCEPT WmoisS
		END-PERFORM
		MOVE 0 TO jourok
		PERFORM WITH TEST AFTER UNTIL jourok = 1
			DISPLAY "Veuillez saisir le jour"
			ACCEPT WjourS
			IF moisfevrier THEN *>Test si le mois choisi est Février
				DIVIDE Wannes BY 4
				GIVING anneediv
				REMAINDER anneedivreste
				IF anneedivreste = 0 THEN 
					DIVIDE Wannes BY 100
					GIVING anneediv
					REMAINDER anneedivreste
					IF anneedivreste > 0 THEN
						IF WjourS < 30 AND > 0 THEN
							MOVE 1 TO jourok
						ELSE
							DISPLAY "Ce jour n'existe pas pour le mois de février de cette année"
						END-IF
					ELSE
						MOVE 0 TO anneedivreste
						DIVIDE Wannes BY 400
						GIVING anneediv
						REMAINDER anneedivreste
						IF anneedivreste = 0 THEN
							IF WjourS < 30 AND > 0 THEN
								MOVE 1 TO jourok
							ELSE
								DISPLAY "Ce jour n'existe pas pour le mois de février de cette année"
							END-IF				
						ELSE
							IF WjourS < 29 AND > 0 THEN
								MOVE 1 TO jourok
							ELSE
								DISPLAY "Ce jour n'existe pas pour le mois de février de cette année"
							END-IF	
						END-IF			
					END-IF
				ELSE
					MOVE 0 TO anneedivreste
					DIVIDE Wannes BY 400
					GIVING anneediv
					REMAINDER anneedivreste
					IF anneedivreste = 0 THEN
						IF WjourS < 30 AND > 0 THEN
							MOVE 1 TO jourok
						ELSE
							DISPLAY "Ce jour n'existe pas pour le mois de février de cette année"
						END-IF				
					ELSE
						IF WjourS < 29 AND > 0 THEN
							MOVE 1 TO jourok
						ELSE
							DISPLAY "Ce jour n'existe pas pour le mois de février de cette année"
						END-IF	
					END-IF
				END-IF *> Fin du test si il s'agit du mois de février
			ELSE 
				IF moispair THEN
					IF Wjours < 31 AND Wjours > 0 THEN 
						MOVE 1 TO jourok
					ELSE
						DISPLAY "Ce jour n'existe pas durant ce mois"
					END-IF
				ELSE
					IF Wjours < 32 AND Wjours > 0 THEN 
						MOVE 1 TO jourok
					ELSE
						DISPLAY "Ce jour n'existe pas durant ce mois"
					END-IF
				END-IF
			END-IF
		END-PERFORM *> Fin des tests pour le jour
		PERFORM WITH TEST AFTER UNTIL WheureS < 24 AND > -1
			DISPLAY "Veuillez saisir l'heure de début de la séance"
			ACCEPT WheureS
		END-PERFORM
		PERFORM WITH TEST AFTER UNTIL WminuteS < 61 AND > -1
			DISPLAY "Veuillez saisir la minute à laquelle commence la séance"
			ACCEPT WminuteS
		END-PERFORM
		.
        
    RECHERCHE_SEANCE.
        DISPLAY "Recherche séance".
    
    SUPPRESSION_SEANCE.
        DISPLAY "Suppression séance".
    
    AJOUT_SALLE.
    
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
              PERFORM WITH TEST AFTER UNTIL WnumS <> ' '
                     DISPLAY 'Quel est le numéro de la salle à ajouter ?'
                     ACCEPT WnumS
              END-PERFORM
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
        DISPLAY "Ajout film".
    
    RECHERCHE_FILM.
        DISPLAY "Recherche film".
    
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
                   READ freservation
                    INVALID KEY 
                       MOVE 1 to Wfin
                    NOT INVALID KEY 
                       DISPLAY "Numéro de reservation déjà existant, saisissez en un nouveau"
                    END-READ
               END-PERFORM
               
               DISPLAY "Saisir le nombre de places à commander"
               ACCEPT WplaceR
                 MOVE WidseanceR TO fr_idseance
                  *> se positionner
                START freservation key = fr_idseance
                invalid key
                    DISPLAY "ERREUR impossible de trouver la seance selectionner"
                not invalid key
                    MOVE 0 TO Wfin
                    *> lecture sur zone indexe 
                 MOVE 0 TO WplaceRestante
                 PERFORM WITH TEST AFTER UNTIL Wfin =1
                    READ freservation NEXT
                    AT END
                        MOVE 1 TO Wfin
                    NOT AT END
                        COMPUTE WnbplaceS = WnbplaceS + fr_place
                    END-READ
                 END-PERFORM
                 OPEN INPUT fsalles
                 MOVE fsea_numsalle TO fsal_num
                 READ fsalles
                    INVALID KEY 
                      DISPLAY "Erreur , la seance n'as pas de salle"
                    NOT INVALID KEY 
                      COMPUTE WplaceRestante = fsal_nbplace - WnbplaceS
                      MOVE 0 TO WmontantR
                      IF WplaceR > WplaceRestante THEN
                        PERFORM WITH TEST AFTER UNTIL Wplace_enfant <= WplaceR AND Wplace_enfant >=0
                         DISPLAY "Saisir le nombre de places enfant "
                         ACCEPT Wplace_enfant
                         COMPUTE WmontantR = WmontantR + WtarifEnfant * Wplace_enfant
                        END-PERFORM
                        COMPUTE Wtampon = WplaceR - Wplace_enfant
                        PERFORM WITH TEST AFTER UNTIL Wplace_abonneR <= Wtampon
                         DISPLAY "Saisir le nombre de places abonnés inferieur ou egale à ",Wtampon
                         ACCEPT Wplace_abonneR
                        END-PERFORM
                        IF fsea_typedif =1 THEN
                          COMPUTE WmontantR = WmontantR + WplaceR * Wtarif3D
                        END-IF
                        MOVE 0 to Wcpt
                        OPEN INPUT fclients
                         PERFORM WITH TEST AFTER UNTIL Wcpt < Wplace_abonneR 
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
                          DISPLAY "nombre de place reserver : ",WplaceR
                          DISPLAY "dont enfant : ",Wplace_enfant
                          DISPLAY "montant total à payer : ",WmontantR
                         ELSE
                            DISPLAY "Erreur inconnue, Impossible d'enregistrer cette reservation"
                         END-IF
                      ELSE
                       DISPLAY "ERREUR, il ne reste que ",WplaceRestante," places pour cette seance"
                       DISPLAY "et vous en demandez",WplaceR
                      END-IF
                 END-READ
                 CLOSE fsalles
                END-START           
              CLOSE freservation
        END-READ
       
        CLOSE fseances
        DISPLAY "---------------fin ajout reservation---------------".
      
    RECHERCHE_RESERVATION.
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
                   DISPLAY "nombre de place reserver : ",WplaceR
                   DISPLAY "dont enfant : ",Wplace_enfant
                   DISPLAY "montant total à payer : ",WmontantR
           END-READ

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
                                 DISPLAY "nombre de place reserver : ",WplaceR
                                 DISPLAY "dont enfant : ",Wplace_enfant
                                 DISPLAY "montant total à payer : ",WmontantR
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
				MOVE 1 TO WfinR
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
