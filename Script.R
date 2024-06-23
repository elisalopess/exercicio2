library(readxl)
dados <- read_excel("\\Users\\elisa\\OneDrive\\Mestrado\\Artigo\\Banco de dados - artigo\\dados_regiao.xlsx")

# dados por região `data.frame`

centro <- ( dados [ dados$bairro =="RECIFE"
                      | dados$bairro =="SANTOAMARO"
                        | dados$bairro =="BOAVISTA"
                          | dados$bairro =="ILHADOLEITE"
                            | dados$bairro =="CABANGA"
                              | dados$bairro =="PAISSANDU"
                                | dados$bairro =="SANTOANTONIO"
                              | dados$bairro =="SAOJOSE"
                            | dados$bairro =="SOLEDADE"
                          | dados$bairro =="COELHOS"
                        | dados$bairro =="ILHAJOANABEZERRA"
                    
                                  ,])


 norte <- ( dados [  dados$bairro =="ARRUDA"
                    | dados$bairro =="CAMPINADOBARRETO"
                      | dados$bairro =="CAMPOGRANDE"
                        | dados$bairro =="ENCRUZILHADA"
                          | dados$bairro =="HIPODROMO"
                        | dados$bairro =="PEIXINHOS"
                      | dados$bairro =="PONTODEPARADA"
                    | dados$bairro =="ROSARINHO"
                      | dados$bairro =="TORREAO"
                        | dados$bairro =="AGUAFRIA"
                          | dados$bairro =="ALTOSANTATEREZINHA"
                        | dados$bairro =="BOMBADOHEMETERIO"
                      | dados$bairro =="CAJUEIRO"
                    | dados$bairro =="FUNDAO"
                      | dados$bairro =="PORTODAMADEIRA"
                        | dados$bairro =="BEBERIBE"
                          | dados$bairro =="DOISUNIDOS"
                        | dados$bairro =="LINHADOTIRO"
                      ,])
                    
                    
 noroeste <- ( dados [ dados$bairro =="AFLITOS"
                        | dados$bairro =="ALTODOMANDU"
                          | dados$bairro =="APIPUCOS"
                            | dados$bairro =="CASAAMARELA"
                              | dados$bairro =="CASAFORTE"
                            | dados$bairro =="DERBY"
                          | dados$bairro =="DOISIRMAOS"
                        | dados$bairro =="ESPINHEIRO"
                      | dados$bairro =="GRACAS"
                        | dados$bairro =="JAQUEIRA"
                          | dados$bairro =="MONTEIRO"
                            | dados$bairro =="PARNAMIRIM"
                              | dados$bairro =="POCODAPANELA"
                            | dados$bairro =="POCO"
                          | dados$bairro =="SANTANA"
                        | dados$bairro =="TAMARINEIRA"
                      | dados$bairro =="SITIODOSPINTOS"
                        | dados$bairro =="ALTOJOSEBONIFACIO"
                          | dados$bairro =="ALTOJOSEDOPINHO"
                            | dados$bairro =="MANGABEIRA"
                              | dados$bairro =="MORRODACONCEICAO"
                            | dados$bairro =="VASCODAGAMA"
                          | dados$bairro =="BREJODAGUABIRABA"
                        | dados$bairro =="BREJODEBEBERIBE"
                      | dados$bairro =="CORREGODOJENIPAPO"
                        | dados$bairro =="GUABIRABA"
                          | dados$bairro =="MACAXEIRA"
                            | dados$bairro =="NOVADESCOBERTA"
                              | dados$bairro =="PASSARINHO"
                            | dados$bairro =="PAUFERRO"
                    ,])
 
 oeste <- (dados [ dados$bairro =="CORDEIRO"
                    | dados$bairro =="ILHADORETIRO"
                      | dados$bairro =="IPUTINGA"
                        | dados$bairro =="MADALENA"
                          | dados$bairro =="PRADO"
                        | dados$bairro =="TORRE"
                      | dados$bairro =="ZUMBI"
                    | dados$bairro =="ENGENHODOMEIO"
                  | dados$bairro =="TORROES"
                    | dados$bairro =="CAXANGA"
                      | dados$bairro =="CIDADEUNIVERSITARIA"
                        | dados$bairro =="VARZEA"
                   ,])
 
                    
 sudoeste <- (dados [ dados$bairro =="AFOGADOS"
                      | dados$bairro =="BONGI"
                        | dados$bairro =="MANGUEIRA"
                          | dados$bairro =="MUSTARDINHA"
                        | dados$bairro =="SANMARTIN"
                      | dados$bairro =="AREIAS"
                    | dados$bairro =="CACOTE"
                      | dados$bairro =="ESTANCIA"
                        | dados$bairro =="JIQUIA"
                          | dados$bairro =="BARRO"
                        | dados$bairro =="COQUEIRAL"
                      | dados$bairro =="CURADO"
                    | dados$bairro =="JARDIMSAOPAULO"
                      | dados$bairro =="SANCHO"
                        | dados$bairro =="TEJIPIO"
                      | dados$bairro =="TOTO"
                      ,])
                    
sul <- (dados [dados$bairro=="BOAVIAGEM"
                | dados$bairro =="BRASILIATEIMOSA"
                  | dados$bairro =="IMBIRIBEIRA"
                    | dados$bairro =="IPSEP"
                  | dados$bairro =="PINA"
                | dados$bairro =="IBURA"
              | dados$bairro =="JORDAO"
                | dados$bairro =="COHAB"
               ,])
                    

ignorados <- (dados [dados$bairro =="IGN",])

# sucesso = obito 
    # falha = NA

x <- sum(table(dados))
y <- sum(table(dados [dados$evolucao =="O",]))


# Taxas ( região/recife )

  tx_geral <- y/x

    tx_centro <- sum(table(centro))/x
  
      tx_noroeste <- sum(table(noroeste))/x
  
        tx_norte <- sum(table(norte))/x
    
      tx_oeste <- sum(table(oeste))/x
    
    tx_sudoeste <- sum(table(sudoeste))/x
    
      tx_sul <- sum(table(sul))/x
  
        tx_ign <- sum(table(ignorados))/x

                
library(ineq)
       
# Sem separar os óbitos 
                
Gini(table(dados))
Gini(table(oeste))
Gini(table(sul))
Gini(table(noroeste))
Gini(table(norte))
Gini(table(sudoeste))
Gini(table(centro))
Gini(table(ignorados))


# Teste com dados 



