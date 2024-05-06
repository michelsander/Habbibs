#Include 'Protheus.ch'
#Include 'FWMVCDEF.ch'
#Include 'RestFul.CH'
#include "TopConn.ch"
#include "TbiConn.ch"

#DEFINE TPOPERACAO "33"

/*/{Protheus.doc} HBARE334
API para integração das apurações fiscais por filial 

@author  Michel Sander
@since   10/10/2021
@type    User Function
@Update 
/*/

User Function HBARE334(aEmpAux)

	Local cTmpAlias   := ""
	Local lArenaOn    := ""
	LOCAL cFuncao     := ""
	Local cBkpFil     := ""
	LOCAL cBkpEmp     := ""

	PRIVATE cChave     := ""
   PRIVATE cUrl       := ""
   PRIVATE cPathInc   := ""
   PRIVATE cPathCan   := ""
   PRIVATE cUser		 := ""
   PRIVATE cPassword	 := ""
	PRIVATE aStatSem   := {}
	PRIVATE aSemaforo  := {}
   PRIVATE aEmpFil    := {}
	PRIVATE cError     := ""
	PRIVATE bError

	DEFAULT aEmpAux := {'01','2501'}

	cEmpAux := aEmpAux[1]
	cFilAux := aEmpAux[2]

	RPCSetType(3)
	RpcSetEnv( cEmpAux, cFilAux )

	dbSelectArea("Z91")
	dbSetOrder(1)

	dbSelectArea("Z96")
	dbSetOrder(1)

	dbSelectArea("ZCD")
	dbSetOrder(1)

	dbSelectArea("ZCE")
	dbSetOrder(1)

	bError  := ErrorBlock({ |oError| cError := oError:Description+CRLF+oError:ErrorStack})	
	lArenaOn := GetMV("FS_AREON")

	If !lArenaOn
		ConOut('[HBARE334] FS_AREON desabilitado.')
		Return
	EndIf

	//=====================================================================================
	// Controle de semaforo - Nao permitir execução de mais de uma instância:
	//=====================================================================================
	cFuncao  := PADR("HBARE334",TamSX3("Z97_ROTINA")[1])
	aStatSem := U_HBVERSEMAF(cFuncao)

	// Verifica se existe registro de semáforo da rotina
	If !aStatSem[1]
		U_HBSMLog("SM",cFilAnt,cFuncao,"499","","","","Codigo chave da rotina "+cFuncao+" nao encontrada na tabela Z97.","499", aStatSem[2], .T. )
		Return
	EndIf

	// Verifica semáforo aberto
	If aStatSem[3] == "2"
		U_HBSMLog("SM",cFilAnt,aStatSem[4],"499","","","","Tentativa de execucao com Semaforo FECHADO","499", aStatSem[2], .T. )
		Return
	EndIf

	// Fecha o semáforo para execução
	aSemaforo := U_HBGRVSEMAF("2", aStatSem[2], aStatSem[4])
	If !aSemaforo[1]
		U_HBSMLog("SM",cFilAnt,aStatSem[4],"499","","","",aSemaforo[2],"499", aStatSem[2], .T. )
		Return
	EndIf

	// Apaga o controle de semáforo fechado 
	U_LimpaZ91("SM",aStatSem[2], Dtos(Date()),'499')

   // Seleciona as filiais para processamento
   cTmpAlias   := GetNextAlias()
	U_HBJOB01A(cTmpAlias,TPOPERACAO)

   While (cTmpAlias)->(!EOF())

		//RETIRAR
		/*
		If AllTrim((cTmpAlias)->Z90_FILIAL) != "0002010001"
		   (cTmpAlias)->(dbSkip())
			Loop
		EndIf 
		*/

      AADD(aEmpFil, { (cTmpAlias)->Z90_EMPRES, (cTmpAlias)->Z90_FILIAL } )
      (cTmpAlias)->(dbSkip())
		
   EndDo

	cBkpFil := cFilAnt 
	cBkpEmp := cEmpAnt 
   (cTmpAlias)->(dbCloseArea())

	Conout("[HBARE334] - Início: " + DtoC(dDataBase) + " - " + Time())

	BEGIN SEQUENCE 
   
      For nX := 1 to Len( aEmpFil )
      
         cEmpAnt   := aEmpFil[nX, 1]
         cFilAnt   := aEmpFil[nX, 2]
         cTpExcl   := AllTrim(Posicione("Z96",1,FWxFilial("Z96")+TPOPERACAO, "Z96_CHAVEC"))

         //Verifica se os parâmetros com a URL / Path de integração rest estão preenchidos
         cUrl        := Alltrim( GetNewPar( "FS_AR33URL", "http://ddnss-vmapp.ddns.com.br:7001") )
         cPathInc    := Alltrim( GetNewPar( "FS_PATH33I","/Arena/API_01/TSM/DistrImp") )
         cPathCan    := Alltrim( GetNewPar( "FS_PATH33C","/Arena/API_01/TSM/DistrImpCanc") )
         cUser			:= Alltrim( GetNewPar( "FS_USRAREN", "Protheus" ) )
         cPassword	:= Alltrim( GetNewPar( "FS_PASWARE", "!@arena@!" ) )
         cDtCorte    := U_HBDTAFECH(TPOPERACAO) 
         cAnoRef     := AllTrim(SubStr(cDtCorte,1,4))
         cMesRef     := AllTrim(SubStr(cDtCorte,5,2))
			cDiaRef     := '01'
			cTipoImp    := AllTrim(GetNewPar("FS_TIMPSF6", "1,3"))
			cTipoIn     := "%SF6.F6_TIPOIMP IN " + FormatIn(cTipoImp, ",") + "%"

			// Verifica LOJA PAGADORA
			If ZCE->(dbSeek(cFilAnt))
				cLojaPg  := AllTrim(ZCE->ZCE_FILPAG)
			Else
				cLojaPg  := AllTrim(GetNewPar("FS_ARFILPG", cFilAnt))
			EndIf 

         If Empty(cUrl) .or. Empty(cPathInc) .or. Empty(cPathCan)
         	Conout("[HBARE334] - " + cEmpAnt + "." + cFilAnt + "- " + "URL e/ou PATH de comunicação não foram configurados. Atualize os parâmetros FS_AR33URL/FS_PATH33I/FS_PATH33C")
            Exit
         EndIf

         Conout("[HBARE334] - " + cEmpAnt + "." + cFilAnt)

			// Seleciona os impostos da SF6
         cAliasTImp := GetNextAlias()
         BEGINSQL Alias cAliasTImp

					SELECT F6_FILIAL FILIAL, 
							 R_E_C_N_O_ F6RECNO, 
							 D_E_L_E_T_ F6DELETE,
							 	(
                        SELECT  TOP 1 Z90_AREFIL
                        FROM    %Table:Z90% Z90
                        WHERE   Z90_FILIAL = %exp:cFilAnt%
                            AND Z90_EMPRES = %exp:cEmpAnt%
                            AND Z90.D_E_L_E_T_ = ' ' 
								) AREFIL 
								FROM %Table:SF6% SF6 
								WHERE SF6.F6_FILIAL = %Exp:cFilAnt% 
								AND SF6.F6_DTARREC > %Exp:cAnoRef+cMesRef+cDiaRef%
								AND %Exp:cTipoIn%
								AND SF6.F6_MSEXP  = ''
								ORDER BY F6_FILIAL, R_E_C_N_O_
         ENDSQL

         Conout("[HBARE334] - SF6- " + cEmpAnt + "." + cFilAnt + "- Ano/Mes Referência "+cAnoRef+"/"+cMesRef)

			// Envia os impostos da SF6
         SF6->(dbSetOrder(1))
			fEnviaSF6(cAliasTImp)
         (cAliasTImp)->(dbCloseArea())

			// Seleciona os impostos da CKR
			cTipoImp    := AllTrim(GetNewPar("FS_TIMPCKR", "1,2"))
			cTipoIn     := "%CKR.CKR_TRIB IN " + FormatIn(cTipoImp, ",") + "%"
         cAliasTImp  := GetNextAlias()
         BEGINSQL Alias cAliasTImp

					SELECT CKR_FILIAL FILIAL, 
							 R_E_C_N_O_ CKRRECNO, 
							 D_E_L_E_T_ CKRDELETE,
							 	(
                        SELECT  TOP 1 Z90_AREFIL
                        FROM    %Table:Z90% Z90
                        WHERE   Z90_FILIAL = %exp:cFilAnt%
                            AND Z90_EMPRES = %exp:cEmpAnt%
                            AND Z90.D_E_L_E_T_ = ' ' 
								) AREFIL 
								FROM %Table:CKR% CKR 
								WHERE CKR.CKR_FILIAL = %Exp:cFilAnt% 
								AND SUBSTRING(CKR.CKR_PER,1,6) > %Exp:AllTrim(SubStr(cDtCorte,1,4))+AllTrim(SubStr(cDtCorte,5,2))%
								AND %Exp:cTipoIn%
								AND CKR.CKR_MSEXP  = ''
								ORDER BY CKR_FILIAL, R_E_C_N_O_
         ENDSQL

         Conout("[HBARE334] - CKR- " + cEmpAnt + "." + cFilAnt + "- Ano/Mes Referência "+cAnoRef+"/"+cMesRef)

			// Envia os impostos PIS e COFINS da tabela CKR
         CKR->(dbSetOrder(1))
			fEnviaCKR(cAliasTImp)
         (cAliasTImp)->(dbCloseArea())

      Next

   RECOVER
         ConOut("[HBARE334] - Erro inesperado no meio do processamento")
         (cAliasTImp)->(dbCloseArea())
   END SEQUENCE 

  	//Restaurando bloco de erro do sistema 
	ErrorBlock(bError)

	//Se houve erro de processamento, será gravado na Z91
	If !Empty(cError)
		U_HBSMLog("SM",cFilAnt,"HBARE334","499","","",,"Processamento da rotina HBARE334 na filial "+AllTrim(cFilAnt)+" Interrompido. Vefique o LOG do serviço de JOB Schedule.","499", "0034", .T. )
		ConOut(" "+CRLF+CRLF)
		ConOut("HBARE334- Houve um erro durante a transmissão na FILIAL: "+AllTrim(cFilAnt)+CRLF+cError)
		ConOut(" "+CRLF+CRLF)
	EndIf
	
	cEmpAnt := cBkpEmp
	cFilAnt := cBkpFil 

	// Abre o semáforo após execução
	aSemaforo := U_HBGRVSEMAF("1", aStatSem[2], aStatSem[4])
	If !aSemaforo[1]
		U_HBSMLog("SM",cFilAnt,aStatSem[4],"499","","","",aSemaforo[2],"499", aStatSem[2], .T. )
	EndIf

	aSize(aStatSem,0)
	aSize(aSemaforo,0)

   Conout("[HBARE334] - Fim: " + DtoC(dDataBase) + " - " + Time())

Return .T.

/*/{Protheus.doc} 
Envia o JSON ao Arena
@author Michel Sander
@since 24/08/2021
@type Function
/*/

Static Function fSendJson(cJson, lCancel)

	Local aHeader  := {}
	Local cPathUso := ""
	Local lIntegr  := .F. 

	AAdd(aHeader, "Content-Type: application/json")
	Aadd(aHeader, 'Authorization: Basic ' + alltrim(OemToAnsi(Encode64(cUser+":"+cPassword))))

	cTpExcl := AllTrim(Posicione("Z96",1,FWxFilial("Z96")+TPOPERACAO, "Z96_CHAVEC"))

	If lCancel 
	   cPathUso := cPathCan 
	Else 
	   cPathUso := cPathInc
	EndIf 

	oRestClient := FWRest():New( cUrl )
	oRestClient:setPath( OemToAnsi(cPathUso) )
	oRestClient:SetPostParams( OemToAnsi(cJson) )
	lIntegr 		:= oRestClient:POST( aHeader )

	If lIntegr

		oJsObj := Nil
		cJsonRet  := oRestClient:GetResult()
		FWJsonDeserialize(DecodeUtf8(cJsonRet),@oJsObj)
		cCodRet := oJsObj:RESULT[1]:STATUS
		cMsgRet := oJsObj:RESULT[1]:MENSAGEM
		cCodErr  := oJsObj:RESULT[1]:CODERRO
		cCodAmb  := oJsObj:RESULT[1]:AMBIENTE
		U_LimpaZ91(TPOPERACAO,cChave, Dtos(Date()),'401',,cFilAnt)
		U_LimpaZ91(cTpExcl,cChave, Dtos(Date()),'401',,cFilAnt)

	ELse

		// Tenta novamente realizar a comunicação com o Arena
		cJsonRet := oRestClient:GetResult()
		If !Empty(cJsonRet)
			oJsObj   := Nil
			FWJsonDeserialize(DecodeUtf8(cJsonRet),@oJsObj)
			cCodRet  := oJsObj:RESULT[1]:STATUS
			cMsgRet  := oJsObj:RESULT[1]:MENSAGEM
			cCodErr  := oJsObj:RESULT[1]:CODERRO
			cCodAmb  := oJsObj:RESULT[1]:AMBIENTE
			cStatus  := cCodRet
			FreeObj( oJsObj )
		Else
			cMsgRet  := AllTrim(oRestClient:GetLastError())
			cJsonRet := ""
			cCodRet  := "401"
			cStatus  := cCodRet
			cCodErr  := "999"
			cCodAmb  := ""
		Endif
	EndIf

	aEnvZ91 := {}  // Matriz para envio dos campos e valores
	AADD(aEnvZ91, {"Z91_FILIAL" , cFilAnt })
	AADD(aEnvZ91, {"Z91_TPOPER" , IIF(lCancel,cTpExcl,TPOPERACAO) })
	AADD(aEnvZ91, {"Z91_CHAVE"  , cChave     })
	AADD(aEnvZ91, {"Z91_JSONOR" , cJson      })
	AADD(aEnvZ91, {"Z91_JSONRE" , cJsonRet   })
	AADD(aEnvZ91, {"Z91_STATUS" , cCodRet    })
	AADD(aEnvZ91, {"Z91_MENSAG" , cMsgRet    })
	AADD(aEnvZ91, {"Z91_PROCES" , ""         })

	If Z91->( FieldPos("Z91_DTORI") ) > 0

		AADD(aEnvZ91, {"Z91_DTORI"  , SF6->F6_DTARREC })
		AADD(aEnvZ91, {"Z91_FORCLI" , SF6->F6_EST  	 })
		AADD(aEnvZ91, {"Z91_TITULO" , SF6->F6_NUMERO  })
		AADD(aEnvZ91, {"Z91_VALOR"  , SF6->F6_VALOR   })
		AADD(aEnvZ91, {"Z91_TAB"    , "SF6"				 })
		AADD(aEnvZ91, {"Z91_INDICE" , "2"				 })
		AADD(aEnvZ91, {"Z91_CHVP"   , SF6->F6_FILIAL+SF6->F6_EST+Dtos(SF6->F6_DTARREC) })
		If !Empty(cCodErr)
			AADD(aEnvZ91, {"Z91_ERRORI"  	, cCodErr					})
		EndIf
		If !Empty(cCodAmb)
			AADD(aEnvZ91, {"Z91_AMB"  	, cCodAmb						})
		EndIf

	EndIf

	lRet := U_HBGRV291(aEnvZ91,Nil,@cMsgRet)
	If !lRet .And. !Empty(cMsgRet)
		Conout("HBARE334 - Erro na gravação da Tabela de Log mensagem:"+cMsgRet )
	EndIf

Return ( lIntegr )

/*/{Protheus.doc} FEnviaSF6
Envia o JSON dos impostos da SF6 ao ARENA
@author Michel Sander
@since 24/10/2021
@type Function
/*/

Static Function fEnviaSF6(cAliasTImp)

	LOCAL lRet := .F.

	While (cAliasTImp)->(!EOF())

		SET DELETED OFF
		SF6->(dbGoto((cAliasTImp)->F6RECNO))
		cChave  := SF6->F6_FILIAL + Dtos(SF6->F6_DTARREC) + SF6->F6_EST + SF6->F6_NUMERO
		cTpExcl := AllTrim(Posicione("Z96",1,FWxFilial("Z96")+TPOPERACAO, "Z96_CHAVEC"))

		If SF6->F6_TIPOIMP == "0"
			cImposto := "=Guia Estadual"
		ElseIf SF6->F6_TIPOIMP == "1"
			cImposto := "=ICMS"
		ElseIf SF6->F6_TIPOIMP == "2"
			cImposto := "=ISS"
		ElseIf SF6->F6_TIPOIMP == "3"
			cImposto := "=ICMS/ST"
		ElseIf SF6->F6_TIPOIMP == "4"
			cImposto := "=FUNRURAL"
		ElseIf SF6->F6_TIPOIMP == "5"
			cImposto := "=SIMP FEDERAL"
		ElseIf SF6->F6_TIPOIMP == "6"
			cImposto := "=FUNDERSUL"
		ElseIf SF6->F6_TIPOIMP == "7"
			cImposto := "=SIMP NACIONAL"
		ElseIf SF6->F6_TIPOIMP == "B"
			cImposto := "=DIFAL"
		Else
			cImposto := "=DESCONHECIDO"
		EndIf 

		If (cAliasTImp)->F6DELETE != '*'

			// Se não foi enviado com sucesso a exclusão da chave, não permite nova inclusão			
			If U_fLerZ91(cFilAnt,cChave,cTpExcl,"401")
				SF6->(MsUnlock())
				(cAliasTImp)->(dbSkip())
				Loop 
			EndIf 

			// Cabecalho
			aDadosCab   := {}
			aadd(aDadosCab, cChave) //id
			aadd(aDadosCab, TPOPERACAO) //tipo
			aadd(aDadosCab, (cAliasTImp)->AREFIL) //Loja Arena
			aadd(aDadosCab, Dtos(SF6->F6_DTARREC)) //data
			aadd(aDadosCab, "Imposto "+SF6->F6_TIPOIMP+cImposto+"-"+Dtos(SF6->F6_DTARREC)) //historico
			aadd(aDadosCab, SF6->F6_VALOR) //valor
			aadd(aDadosCab, cLojaPg) //lojapg
			oObjCab := CabecalhoApuracao():New(aDadosCab)
			oObjCab:AddItem()

			//Itens
			aDadosItens := {}
			aadd(aDadosItens, cChave+"x01") //iditem
			aadd(aDadosItens, cChave) //idicab
			cIdRd := ""
			ZCD->(dbSetOrder(2))
			If ZCD->(dbSeek(xFilial("ZCD")+SF6->F6_TIPOIMP))
				cIdRd := ZCD->ZCD_CODRRR
			EndIf 
			aadd(aDadosItens, cIdRd) //idrd
			aadd(aDadosItens, SF6->F6_VALOR) //vlritem
			aadd(aDadosItens, "") //c centro de custo
			aadd(aDadosItens, "") // nome do centro custo
			aadd(aDadosItens, "Imposto "+SF6->F6_TIPOIMP+cImposto+"-"+Dtos(SF6->F6_DTARREC)) //histitem
			oObjTit := ItensApuracao():New(aDadosItens)
			AADD(oObjCab:itens, oObjTit)
			cJson := Lower(OemToAnsi(FWJsonSerialize(oObjCab,.F.,.T.)))
			lRet  := FSendJson(cJson, .F.)

		Else 

			// Caso não tenha sido enviado a inclusão de imposto, não envia cancelamento
			If Empty(SF6->F6_XINTARE)
				U_LimpaZ91(TPOPERACAO,cChave, Dtos(Date()),'401', .F., cFilAnt) // .T. Aciona o Like / .F. Busca chave exata
				Reclock("SF6",.F.)
				SF6->F6_MSEXP   := Dtos(Date())
				SF6->F6_XINTARE := Date()
				SF6->(MsUnlock())
				(cAliasTImp)->(dbSkip())
				Loop 
			EndIf 

			cJson := '{'
			cJson += '"id": "'+cChave+'",'
			cJson += '"tipo": "'+TPOPERACAO+'",'
			cJson += '"loja": "'+AllTrim((cAliasTImp)->AREFIL)+'"'
			cJson += '}'
			lRet  := FSendJson(cJson, .T.)

		EndIf 

		If lRet 
			Reclock("SF6",.F.)
			SF6->F6_MSEXP   := Dtos(Date())
			SF6->F6_XINTARE := Date()
			SF6->(MsUnlock())
		EndIf 

		SET DELETED ON 
		(cAliasTImp)->(dbSkip())

	EndDo

Return

/*/{Protheus.doc} FEnviaCKR
Envia o JSON dos impostos PIS e COFINS da CKR ao ARENA
@author Michel Sander
@since 24/10/2021
@type Function
/*/

Static Function fEnviaCKR(cAliasTImp)

	LOCAL lRet := .F.

	While (cAliasTImp)->(!EOF())

		SET DELETED OFF
		CKR->(dbGoto((cAliasTImp)->CKRRECNO))
		cChave := CKR->CKR_FILIAL + Dtos(CKR->CKR_PER) + CKR->CKR_TRIB
		cTpExcl := AllTrim(Posicione("Z96",1,FWxFilial("Z96")+TPOPERACAO, "Z96_CHAVEC"))

		If CKR->CKR_TRIB == "1"
			cImposto := "=PIS"
		ElseIf CKR->CKR_TRIB == "2"
			cImposto := "=COFINS"
		Else
			cImposto := "=DESCONHECIDO"
		EndIf 

		If (cAliasTImp)->CKRDELETE != '*'

			// Se não foi enviado com sucesso a exclusão da chave, não permite nova inclusão
			If U_fLerZ91(cFilAnt,cChave,cTpExcl,"401")
				CKR->(MsUnlock())
				(cAliasTImp)->(dbSkip())
				Loop 
			EndIf

			// Cabecalho
			aDadosCab   := {}
			aadd(aDadosCab, cChave) //id
			aadd(aDadosCab, TPOPERACAO) //tipo
			aadd(aDadosCab, (cAliasTImp)->AREFIL) //Loja Arena
			aadd(aDadosCab, Dtos(CKR->CKR_PER)) //data
			aadd(aDadosCab, "Imposto "+CKR->CKR_TRIB+cImposto+"-"+Dtos(CKR->CKR_PER)) //historico
			aadd(aDadosCab, CKR->CKR_CREC) //valor
			aadd(aDadosCab, cLojaPg) //lojapg
			oObjCab := CabecalhoApuracao():New(aDadosCab)
			oObjCab:AddItem()

			//Itens
			aDadosItens := {}
			aadd(aDadosItens, cChave+"x01") //iditem
			aadd(aDadosItens, cChave) //idicab
			cIdRd := ""
			ZCD->(dbSetOrder(3))
			If ZCD->(dbSeek(xFilial("ZCD")+CKR->CKR_TRIB))
				cIdRd := ZCD->ZCD_CODRRR
			EndIf 
			aadd(aDadosItens, cIdRd) //idrd
			aadd(aDadosItens, CKR->CKR_CREC) //vlritem
			aadd(aDadosItens, "") //ccentro de custo
			aadd(aDadosItens, "") //nome do centro de custo
			aadd(aDadosItens, "Imposto "+CKR->CKR_TRIB+cImposto+"-"+Dtos(CKR->CKR_PER)) //histitem
			oObjTit := ItensApuracao():New(aDadosItens)
			AADD(oObjCab:itens, oObjTit)
			cJson := Lower(OemToAnsi(FWJsonSerialize(oObjCab,.F.,.T.)))
			lRet  := FSendJson(cJson, .F.)

		Else 

			// Caso não tenha sido enviado a inclusão de imposto, não envia cancelamento
			If Empty(CKR->CKR_XINTAR)
				U_LimpaZ91(TPOPERACAO,cChave, Dtos(Date()),'401', .F., cFilAnt) // .T. Aciona o Like / .F. Busca chave exata
				Reclock("CKR",.F.)
				CKR->CKR_MSEXP   := Dtos(Date())
				CKR->CKR_XINTAR  := Date()
				CKR->(MsUnlock())
				(cAliasTImp)->(dbSkip())
				Loop 
			EndIf 

			cJson := '{'
			cJson += '"id": "'+cChave+'",'
			cJson += '"tipo": "'+TPOPERACAO+'",'
			cJson += '"loja": "'+AllTrim((cAliasTImp)->AREFIL)+'"'
			cJson += '}'
			lRet  := FSendJson(cJson, .T.)

		EndIf 

		If lRet 
			Reclock("CKR",.F.)
			CKR->CKR_MSEXP   := Dtos(Date())
			CKR->CKR_XINTAR  := Date()
			CKR->(MsUnlock())
		EndIf 

		SET DELETED ON 
		(cAliasTImp)->(dbSkip())

	EndDo

Return

/*/{Protheus.doc} 
Classes para geração dos objetos na estrutura do Json

@author  Michel Sander
@since   10/10/2021
@type    Function
/*/

Class CabecalhoApuracao
	data id
	data tipo
	data loja
	data data
	data historico
	data valor
	data lojapg
	data itens
	Method New(aDados)
	Method AddItem()
EndClass

Method AddItem() Class CabecalhoApuracao
	self:itens := {}
Return

Method New(aDados) Class CabecalhoApuracao
	self:id             := AllTrim(aDados[01])
	self:tipo           := alltrim(aDados[02])
	self:loja           := alltrim(aDados[03])
	self:data           := alltrim(aDados[04])
	self:historico      := alltrim(aDados[05])
	self:valor          := aDados[06]
	self:lojapg			  := AllTrim(aDados[07])
	self:itens   		  := {}
Return

Class ItensApuracao
	data iditem
	data idcab
	data idrd
	data vlritem
	data ccusto
	data nomecusto
	data histitem
	Method New(aDados)
EndClass

Method New(aDados) Class ItensApuracao

	self:iditem       := aDados[01]
	self:idcab      	:= aDados[02]
	self:idrd       	:= aDados[03]	
	self:vlritem      := aDados[04]	
	self:ccusto       := aDados[05]
	self:nomecusto    := aDados[06]	
	self:histitem     := aDados[07]
	
Return

