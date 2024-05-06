#INCLUDE "protheus.ch"

/*/ HBARE340()
Função chamada pelo Ponto de Entrada MT103FIM
para atualizar os flags do Arena no estorno de classificação de nota fiscal de entrada

@project 	Habibs
@author 	   Michel Sander
@since 		19/08/2021
@version 	P12.1.17
@type 		User Function
@history
/*/ 

User Function HBARE340()

	LOCAL cJson	     := ""
	LOCAL cAreFil    := ""
	LOCAL xTpOp      := ""
	LOCAL cVerOper   := ""
	LOCAL cUltSeq    := ""
	LOCAL cCodRet    := ""
	LOCAL cMsgRet    := ""
	LOCAL cStatus    := ""
	LOCAL cCodErr    := ""
	LOCAL cCodAmb    := ""
	LOCAL cLastError := ""
	LOCAL cJsonRet   := ""
   LOCAL cMsgZ91    := ""
	LOCAL cChave     := ""
	LOCAL lIntegr    := .F.
	LOCAL lRet       := .F.
   LOCAL lProxSeq   := .F.
	LOCAL oJson      := NIL
	LOCAL oJsonRet   := NIL
	LOCAL oRestClient:= NIL
	LOCAL cUrlIntDel := SuperGetMv( "FS_ARE0104" , , "http://ddnss-vmapp.ddns.com.br:7001/Arena/API_01/TSM/docEntradaCanc" ) // URL Arena - Exclusão Documento de Entrada //"http://Localhost:8082/rest/ZTSTWSREST"
	LOCAL cPathXML	  := SuperGetMv( "FS_ARE0105" , , "\\file_server\arena\doc_entrada\xml\" ) // Endereço/Pasta File Server Arquivos XML
	LOCAL cUser		  := SuperGetMv( "FS_ARE0106" , , "Protheus" )     // Login WS Arena - Usuário
	LOCAL cPass		  := SuperGetMv( "FS_ARE0107" , , "!@arena@!" )    // Login WS Arena - Senha
	LOCAL cPathLog	  := SuperGetMv( "FS_ARE0108" , ,"\IntArena\Log")  // Pasta Log Integração Arena - Mensagem de Retorno
   LOCAL cAliasZ97  := GetNextAlias()
	LOCAL aEnvZ91    := {}
	LOCAL aHeadStr   := {}

	If Right(cPathLog,1) <> "\"
		cPathLog += "\"
	EndIf

	If SF1->( FieldPos("F1_XCANCEL") ) > 0 .And. SF1->( FieldPos("F1_XARENA") ) > 0 .And. SF1->( FieldPos("F1_XULTCHV") )

      // Verifica se existe semáforo fechado para a rotina
      BEGINSQL Alias cAliasZ97

         SELECT Z97_STATUS FROM %Table:Z97% Z97
         WHERE  Z97_FILIAL = %exp:FWxFilial("Z97")% 
         AND    Z97_ROTINA = 'HBARE010'
         AND    Z97.%NotDel%

      ENDSQL

		// Salva a chave de cancelamento para próxima recorrência 
      cChave := AllTrim(SF1->(IIF(Empty(F1_DUPL),"02","01")+F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_FORMUL))
      If (cAliasZ97)->Z97_STATUS == '2'
         Reclock("SF1",.F.)
			SF1->F1_XARENA  := "C"
         SF1->F1_XCANCEL := "S"
         SF1->F1_XULTCHV := cChave
         SF1->(MsUnlock())
         (cAliasZ97)->(dbCloseArea())
         Return
      Endif 
      (cAliasZ97)->(dbCloseArea())

		If AllTrim(SF1->F1_TIPO) == "D"
			xTpOp    := "D"
			cVerOper := Posicione("Z96",1,FWxFilial("Z96")+"D", "Z96_CHAVEC")
		Else
			xTpOp    := If( Empty(SF1->F1_DUPL), "02", "01")
			cVerOper := Posicione("Z96",1,FWxFilial("Z96")+xTpOp, "Z96_CHAVEC")
		Endif

		// Verifica se existe inclusão ativa no Arena para enviar estorno de classificação 
		If !U_fLerZ91(SF1->F1_FILIAL, cChave, xTpOp, "200")
         Reclock("SF1",.F.)
         SF1->F1_XCANCEL := ""
         SF1->F1_XULTCHV := ""
         SF1->(MsUnlock())
			Return
		EndIf

		// Verifica histórico de envios para permitir novo estorno de classificação
		cUltSeq := U_SeqChvZ91(SF1->F1_FILIAL, xTpOp, cChave, .F.)
		If !U_fHistZ91(SF1->F1_FILIAL, cChave, "200", xTpOp, cVerOper, cUltSeq)
			U_LimpaZ91(xTpOp    ,cChave, Dtos(Date()),'401', .F.) // .T. Aciona o Like / .F. Busca chave exata
			U_LimpaZ91(cVerOper ,cChave, Dtos(Date()),'401', .F.) // .T. Aciona o Like / .F. Busca chave exata
			U_LimpaZ91("DT"	  ,cChave, Dtos(Date()),'401', .F.) // .T. Aciona o Like / .F. Busca chave exata
         Reclock("SF1",.F.)
         SF1->F1_XCANCEL := ""
         SF1->F1_XULTCHV := ""
         SF1->(MsUnlock())
			Return
		EndIf

      // Monta o JSON de CANCELAMENTO
		cAreFil        := AllTrim(Posicione("Z90",1,SF1->F1_FILIAL,"Z90_AREFIL"))
		oJson          := JsonObject():new()
		oJson["id"	]	:= cChave
		oJson["loja"]	:= cAreFil
		cJson	:= oJson:ToJson()
		FreeObj( oJson )

      // Envia o registro pela API
		aAdd( aHeadStr , 'Content-Type: application/json' )
		aAdd( aHeadStr , "Authorization: Basic " + Encode64(cUser+":"+cPass))
		oRestClient := FWRest():New( cUrlIntDel )
		oRestClient:setPath( cPathXML )
		oRestClient:SetPostParams( cJson )
		oRestClient:nTimeOut := 60000
		lIntegr    := oRestClient:POST( aHeadStr )
		cLastError := AllTrim(oRestClient:GetLastError())
		oJsonRet   := NIL
		cJsonRet   := oRestClient:cResult //oRestClient:GetResult()

      // Tratamento do retorno do Arena
		If lIntegr
			FWJsonDeserialize(DecodeUtf8(cJsonRet),@oJsonRet)
			cCodRet  := oJsonRet:RESULT[1]:STATUS
			cMsgRet  := oJsonRet:RESULT[1]:MENSAGEM
			cStatus  := oJsonRet:RESULT[1]:STATUS
			cCodErr  := oJsonRet:RESULT[1]:CODERRO
			cCodAmb  := oJsonRet:RESULT[1]:AMBIENTE
		Else
			cMsgRet  := cLastError
			cCodRet  := "401"
			cStatus  := cCodRet
		EndIf

      // Atualiza as flags de envio
      Reclock("SF1",.F.)
      SF1->F1_XARENA := "C"
		If cCodRet == "200"
			U_LimpaZ91(cVerOper, cChave, Dtos(Date()),'401', .F.)
			U_LimpaZ91("DT"	 , cChave, Dtos(Date()),'401', .F.)
         SF1->F1_XCANCEL := ""
         SF1->F1_XULTCHV := ""
      Else
         SF1->F1_XCANCEL := "S"
         SF1->F1_XULTCHV := cChave
		EndIf
      SF1->(MsUnlock())

      // Prepara o registro de log de envio ao Arena
		AADD(aEnvZ91, {"Z91_FILIAL" , xFilial("Z91")})
		AADD(aEnvZ91, {"Z91_TPOPER" , cVerOper		})
		AADD(aEnvZ91, {"Z91_CHAVE"  , cChave 		})
		AADD(aEnvZ91, {"Z91_JSONOR" , cJson 		})
		AADD(aEnvZ91, {"Z91_DTOPER" , Date() 		})
		AADD(aEnvZ91, {"Z91_JSONRE" , cJsonRet		})
		AADD(aEnvZ91, {"Z91_STATUS" , cStatus		})
		AADD(aEnvZ91, {"Z91_MENSAG" , cMsgRet		})
		AADD(aEnvZ91, {"Z91_PROCES" , IIF(cStatus $ '200|201', '4', '1') })

		If Z91->( FieldPos("Z91_DTORI") ) > 0

			AADD(aEnvZ91, {"Z91_DTORI"  , SF1->F1_DTDIGIT	})
			AADD(aEnvZ91, {"Z91_FORCLI" , SF1->F1_FORNECE	})
			AADD(aEnvZ91, {"Z91_TITULO" , SF1->F1_DOC			})
			AADD(aEnvZ91, {"Z91_VALOR"  , SF1->F1_VALBRUT	})
			AADD(aEnvZ91, {"Z91_TAB"	 , "SF1"					})
			AADD(aEnvZ91, {"Z91_INDICE" , "1"					})
			AADD(aEnvZ91, {"Z91_CHVP"   , SF1->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA+F1_TIPO)	})
			If !Empty(cCodErr)
				AADD(aEnvZ91, {"Z91_ERRORI"  	, cCodErr					})
			EndIf
			If !Empty(cCodAmb)
				AADD(aEnvZ91, {"Z91_AMB"  	, cCodAmb						})
			EndIf

		EndIf

		// Controle de sequenciamento da mesma chave
		If Z91->( FieldPos("Z91_SEQCHV") ) > 0
			AADD(aEnvZ91, {"Z91_SEQCHV"  	, U_SeqChvZ91(xFilial("Z91"),If( Empty(SF1->F1_DUPL), "02", "01"), cChave, lProxSeq )	})
		EndIf

      // Grava o log de envio ao Arena
		U_HBGRV291(aEnvZ91,Nil,@cMsgZ91)

	EndIf

Return
