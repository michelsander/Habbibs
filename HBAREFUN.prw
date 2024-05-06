#INCLUDE "PROTHEUS.CH"
#Include "FWMVCDef.ch"
#INCLUDE "RWMAKE.CH"
#INCLUDE "TBICONN.CH"
#Include "AP5MAIL.CH"
#INCLUDE "TOPCONN.CH"

Static oEnvia     := Nil    //Object FwRest
Static oRetorno   := Nil    //Object JsonObject com o retorno do Arena 

/*/{Protheus.doc} HBRetJson 
Função que verifica o JSON de retorno de processamento com o Arena
@project 	Habibs
@author 	   Michel Sander
@since 		23/12/2020
@version 	P12.1.17
@type 		Static Function
@history 	
/*/ 

User function HBRetJson(cArqJson)

	Local oJson
	Local cRetJ
	Local cStatus := ""
	Local cTag
	Local aRet := {}

	oJson := JsonObject():New()
	cRetJ := oJson:FromJson(cArqJson)

	If ValType(cRetJ) == "C"
		AADD(aRet, .F.)
		AADD(aRet, "HBARE320- Retorno: Erro na captura do arquivo JSON de retorno do Arena")
		Return ( aRet )
	Endif

	cTag := oJson:GetJsonObject("result")
	If ValType( cTag ) == "A"
		cStatus := cTag[1]:GetJsonObject('status')
		AADD( aRet, .T.)
		AADD( aRet, AllTrim(cStatus))
	Else
		AADD( aRet, .F.)
		AADD( aRet, AllTrim(cArqJson))
	EndIf

Return ( aRet )


/*/{Protheus.doc} HBVERSEMAF
Função para verificar status do semáforo de integração do ARENA
@project 	Habibs
@author 	   Michel Sander
@since 		21/01/2021
@version 	P12.1.17
@type 		Static Function
@history
/*/ 
User Function HBVERSEMAF(cFunUso)
	Local cAliasZ97 := GetNextAlias()
	Local aArea     := GetArea()
	Local aControle := { .F., "", "", "" }
	Local cChaveZ97 := ""
	Local aMonitor  := GetUserInfoArray()

	// Verifica se existe semáforo fechado para a rotina
	BEGINSQL Alias cAliasZ97
		SELECT Z97_FILIAL, Z97_CHAVE, Z97_ROTINA, Z97_STATUS, Z97_JOBID FROM %Table:Z97% Z97
		WHERE  Z97_FILIAL = %exp:FWxFilial("Z97")% 
		AND    Z97_ROTINA = %exp:cFunUso%
		AND    Z97.%NotDel%
	ENDSQL
	cChaveZ97 := (cAliasZ97)->Z97_FILIAL + (cAliasZ97)->Z97_CHAVE + (cAliasZ97)->Z97_ROTINA
	If (cAliasZ97)->(!Eof())
		If (cAliasZ97)->Z97_STATUS == '2'

			// Verifica se a rotina está em processamento no momento
			cFunUso := Alltrim(cFunUso)
			If (cAliasZ97)->Z97_JOBID > 0
				If aScan(aMonitor, { |aJOB| aJOB[3] == (cAliasZ97)->Z97_JOBID })
					aControle := { .T., (cAliasZ97)->Z97_CHAVE, (cAliasZ97)->Z97_STATUS, (cAliasZ97)->Z97_ROTINA }
					(cAliasZ97)->(dbCloseArea())
					RestArea(aArea)
					Return ( aControle )
				Endif
			EndIf

			// Reabre o semáforo se a rotina não estiver processando no momento
			Z97->(dbSetOrder(1))
			If Z97->(dbSeek(cChaveZ97))
				Reclock("Z97",.F.)
				Z97->Z97_STATUS := "1"	// Reabre o semáforo
				Z97->(MsUnlock())
				(cAliasZ97)->(dbCloseArea())
				RestArea(aArea)
				Return ( { .T., Z97->Z97_CHAVE, Z97->Z97_STATUS, Z97->Z97_ROTINA } )
			EndIf

		EndIf

		aControle := { .T., (cAliasZ97)->Z97_CHAVE, (cAliasZ97)->Z97_STATUS, (cAliasZ97)->Z97_ROTINA }

	EndIf

	(cAliasZ97)->(dbCloseArea())
	RestArea(aArea)

Return ( aControle )

/*/{Protheus.doc} HBGRVSEMAF 
Função para controlar o semáforo das rotinas de integração do ARENA
@project 	Habibs
@author 	   Michel Sander
@since 		21/01/2021
@version 	P12.1.17
@type 		Static Function
@history 	
/*/ 

User Function HBGRVSEMAF(cStatUso, cCodUso, cFunUso)

	Local cBusca   := FWxFilial('Z97')+cCodUso+cFunUso
	Local aAreaZ97 := Z97->(GetArea())
	Local aRet     := { .F., "" }
	Local cMgsRet  := ""
	Local cErrSem  := ""
	Local bErrSem

	PRIVATE INCLUI := .F.
	PRIVATE ALTERA := .T.

	bErrSem   := ErrorBlock({ |oError| cErrSem := oError:Description})
	oModelZ97 := FWLoadModel('HBARE140')

	BEGIN SEQUENCE

		Z97->(dbSetOrder(1))
		If !Z97->(dbSeek(cBusca))
			Z97->(RestArea(aAreaZ97))
			Return( { .F., "Registro de semáforo "+cBusca+" não encontrado na tabela Z97." } )
		EndIf
		oModelZ97:SetOperation(MODEL_OPERATION_UPDATE)
		oModelZ97:Activate()

		// Atualiza o semáforo
		If cStatUso == "2"
			oModelZ97:SetValue('Z97MASTER', 'Z97_DTINI' , Date())
			oModelZ97:SetValue('Z97MASTER', 'Z97_HRINI' , Time())
			oModelZ97:SetValue('Z97MASTER', 'Z97_DTFIM' , Ctod(""))
			oModelZ97:SetValue('Z97MASTER', 'Z97_HRFIM' , "")
			oModelZ97:SetValue('Z97MASTER', 'Z97_STATUS', cStatUso)
			oModelZ97:SetValue('Z97MASTER', 'Z97_JOBID' , ThreadID())
		Else
			oModelZ97:SetValue('Z97MASTER', 'Z97_DTFIM' , Date())
			oModelZ97:SetValue('Z97MASTER', 'Z97_HRFIM' , Time())
			oModelZ97:SetValue('Z97MASTER', 'Z97_STATUS', cStatUso)
			oModelZ97:SetValue('Z97MASTER', 'Z97_JOBID' , 0)
		EndIf

		// Valida os dados e atualiza
		If oModelZ97:VldData()
			oModelZ97:CommitData()
			If cStatUso == "2"
				aRet := { .T., "Semaforo foi FECHADO" }
			Else
				aRet := { .T., "Semaforo foi ABERTO" }
			EndIf
		Else
			cMgsRet := 'Erro ao gerar semáforo'+CRLF+CRLF+oModelZ97:GetErrorMessage()[6]
			aRet    := { .F., cMgsRet }
		EndIf

	END SEQUENCE

	oModelZ97:Deactivate()

	//Restaurando bloco de erro do sistema
	ErrorBlock(bErrSem)

	If !Empty(cErrSem)
		Z97->(dbSetOrder(1))
		If Z97->(dbSeek(cBusca))
			If Z97->Z97_STATUS == '2'
				Reclock("Z97",.F.)
				Z97->Z97_STATUS := '1'
				Z97->(MsUnlock())
				aRet := { .F., "Semáforo REABERTO por falha de gravação na Z97" }
			EndIf
		EndIf
	Else
		Z97->(RestArea(aAreaZ97))
	EndIf

Return ( aRet )

/*/{Protheus.doc} HBSMLOG
Gera o LOG de semáforo do processamento do ARENA

@author Michel Sander
@since 21/01/2021
@version P12.1.017 
/*/

User Function HBSMLog(cTpOper,cAreFil,cRot,cSt,cJson,xPostRet,cChave,cMsgErro,cStatus,xChave, lManual )

	Local aEnvZ91 := {}  // Matriz para envio dos campos e valores
	Local cMsgRet := "" // Retorno da Mensagem de erro de gravação da Z91
	Local lRet     := .T.

	DEFAULT lManual := .F.

	If !lManual

		AADD(aEnvZ91, {"Z91_FILIAL" , xFilial("Z91")})
		AADD(aEnvZ91, {"Z91_TPOPER" , cTpOper		})
		AADD(aEnvZ91, {"Z91_CHAVE"  , xChave 		})
		AADD(aEnvZ91, {"Z91_JSONOR" , cJson 		})
		AADD(aEnvZ91, {"Z91_DTOPER" , Date() 		})
		AADD(aEnvZ91, {"Z91_JSONRE" , xPostRet		})
		AADD(aEnvZ91, {"Z91_STATUS" , cStatus		})
		AADD(aEnvZ91, {"Z91_MENSAG" , cMsgErro		})
		AADD(aEnvZ91, {"Z91_PROCES" , IIF(cStatus $ '200|201', '4', '1') })

		lRet := U_HBGRV291(aEnvZ91,Nil,@cMsgRet,xChave)

		If !lRet .And. !Empty(cMsgRet)
			Conout("HBARE080 - Erro na gravação da Tabela de Log mensagem:"+cMsgRet )
		EndIf

	Else

		//Z91_FILIAL, Z91_TPOPER, Z91_CHAVE, Z91_DTOPER, Z91_HROPER, R_E_C_N_O_, D_E_L_E_T_
		xData := Dtos(Date())
		xHora := SubStr(Time(),1,5)

		Z91->(dbSetOrder(1))
		If Z91->(dbSeek(xFilial()+cTpOper+xChave+xData+xHora))
			Reclock("Z91",.F.)
			Z91->Z91_DTOPER := Stod(xData)
			Z91->Z91_HROPER := xHora
			Z91->Z91_JSONOR := cJson
			Z91->Z91_JSONRE := xPostRet
			Z91->Z91_STATUS := cStatus
			Z91->Z91_MENSAG := cMsgErro
			Z91->Z91_PROCES := IIF(cStatus $ '200|201', '4', '1')
			Z91->Z91_DTCORR := Stod(xData)
			Z91->(MsUnlock())
		Else
			Reclock("Z91",.T.)
			Z91->Z91_FILIAL := FWxFilial("Z91")
			Z91->Z91_TPOPER := cTpOper
			Z91->Z91_CHAVE  := xChave
			Z91->Z91_DTOPER := Stod(xData)
			Z91->Z91_HROPER := xHora
			Z91->Z91_JSONOR := cJson
			Z91->Z91_JSONRE := xPostRet
			Z91->Z91_STATUS := cStatus
			Z91->Z91_MENSAG := cMsgErro
			Z91->Z91_PROCES := IIF(cStatus $ '200|201', '4', '1')
			Z91->Z91_DTCORR := Stod(xData)
			Z91->(MsUnlock())
		EndIf

	EndIf

Return

/*/{Protheus.doc} LimpaZ91
Limpa os erros de semáforo do LOG ARENA  

@author Michel Sander
@since 21/01/2021
@version P12.1.017
/*/ 

User Function LimpaZ91(xOper, xChave, xData, xStatus, lLike, cFilZ90)

	Local 	cSQL := ""
	DEFAULT lLike := .F.
	DEFAULT cFilZ90 := cFilAnt

	cSQL := "UPDATE "+RetSQLName("Z91")+" SET D_E_L_E_T_='*', R_E_C_D_E_L_=R_E_C_N_O_ WHERE "
	If AllTrim(xOper) != "SM"
		cSQL += "Z91_FILIAL ='"+FWxFilial("Z91", cFilZ90)+"' AND "
	EndIf
	cSQL += "Z91_TPOPER ='"+xOper +"' AND "
	If lLike
		cSQL += "Z91_CHAVE LIKE '%"+xChave+"%' AND "
	Else
		cSQL += "Z91_CHAVE  ='"+xChave+"' AND "
	EndIf
	If xStatus == "401"
		cSQL += "Z91_STATUS IN ('401','999') "
	Else
		cSQL += "Z91_STATUS ='"+xStatus+"'"
	EndIf

	//ConOut(cSQL)

	TcSQLExec(cSQL)

Return NIL

/*/{Protheus.doc} fLerZ91
Função para procurar a chave de inclusão na tabela de LOG do Arena
@project 	Habibs
@author 	   Michel Sander
@since 		22/02/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function fLerZ91(cxFilial,cxChave,cxTpoper,cxStatus,lxChvParc)

	Local lRet 	 	:= .F.
	Local cQuery 	:= GetNextAlias()
	Local aArea  	:= GetArea()
	Local cInChave := ""

	Default cxStatus  := "200"
	Default lxChvParc := .F.

	If lxChvParc

		cInChave := "%SUBSTRING(Z91_CHAVE,1,"+AllTrim(Str(Len(AllTrim(cxChave))))+") = '"+AllTrim(cxChave)+"'%"

		BEGINSQL Alias cQuery

			SELECT R_E_C_N_O_ RECNOZ91 FROM %Table:Z91% Z91
						WHERE Z91_FILIAL = %Exp:cxFilial%
						AND Z91_TPOPER   = %Exp:cxTpoper%
						AND Z91_STATUS   = %Exp:cxStatus%
						AND %Exp:cInChave%
						AND Z91.%NotDel%

		ENDSQL

	Else

		BEGINSQL Alias cQuery

			SELECT R_E_C_N_O_ RECNOZ91 FROM %Table:Z91% Z91
						WHERE Z91_FILIAL = %Exp:cxFilial%
						AND Z91_TPOPER   = %Exp:cxTpoper%
						AND Z91_CHAVE    = %Exp:cxChave%
						AND Z91_STATUS   = %Exp:cxStatus%
						AND Z91.%NotDel%

		ENDSQL

	EndIf 

	lRet := !(cQuery)->(Eof())
	(cQuery)->(dbCloseArea())

	RestArea(aArea)

Return (lRet)

/*/{Protheus.doc} HBJOB01A
Função para bloquei de filiais no processamento do ARENA 
@project 	Habibs
@author 	   Luciano Garcia
@since 		22/02/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function HBJOB01A(cAliasZ90,cTpInt)

	BEGINSQL ALIAS cAliasZ90
            SELECT  Z90_FILIAL, Z90_EMPRES, Z90_AREEMP, Z90_AREFIL, Z90_NOMFIL, Z90_GRUPO
            FROM    %table:Z90% Z90 
            WHERE   Z90.%notDel% 
					AND Z90.Z90_FILIAL NOT IN
							(
								SELECT ZCA.ZCA_CODFIL 
								FROM %table:ZCA% ZCA 
								WHERE ZCA.%notDel%
									AND ZCA_FILIAL = %xFilial:ZCA%
									AND ZCA.ZCA_CHAVE= %exp:cTpInt% 
									AND ZCA.ZCA_STATUS='2' 
							)
            
			ORDER BY Z90.Z90_EMPRES, Z90.Z90_FILIAL
	ENDSQL

	//ConOut(GetLastQuery()[2])

Return(cAliasZ90)

/*/{Protheus.doc} HBDTAFECH
Função de busca da data de corte para processamento da integração com o ARENA
@project 	Habibs
@author 	   Michel Sander
@since 		13/03/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function HBDTAFECH(xTpOper)

	LOCAL cDtaCorte := ""
	LOCAL aArea     := GetArea()
	LOCAL aAreaZ96  := Z96->(GetArea())
	LOCAL aAreas    := { aArea, aAreaZ96 }

	Z96->(dbSetOrder(1))
	If Z96->(dbSeek(xFilial()+AllTrim(xTpOper)))
		cDtaCorte := AllTrim(DTOS(Z96->Z96_DTFECH))
	EndIf

	AEval( aAreas, {|x| RestArea(x)} )

Return ( cDtaCorte )

/*/{Protheus.doc} HBPERFECH
Função de busca da data inicial e final para processamento da integração com o ARENA
@project 	Habibs
@author 	   Michel Sander
@since 		06/08/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function HBPERFECH(xTpOper)

	LOCAL cDtaCorte := ""
	LOCAL aArea     := GetArea()
	LOCAL aAreaZ96  := Z96->(GetArea())
	LOCAL aAreas    := { aArea, aAreaZ96 }
	LOCAL aRetPer   := {}

	Z96->(dbSetOrder(1))
	If Z96->(dbSeek(xFilial()+AllTrim(xTpOper)))
		AADD( aRetPer, AllTrim(DTOS(Z96->Z96_PROCIN)) )
		AADD( aRetPer, AllTrim(DTOS(Z96->Z96_PROCFN)) )
	EndIf

	AEval( aAreas, {|x| RestArea(x)} )

Return ( aRetPer )

/*/{Protheus.doc} HBAREMAIL
Função para enviio de e-mails
@project 	Habibs
@author 	   Michel Sander
@since 		14/03/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function HBAREMAIL(cPara,cCopia,cAssunto,cMensagem,aArquivos)

	Local lMailAuth		:= SuperGetMv("MV_RELAUTH",,.F.)
	Local nPorta 			:= 587
	Local aRet        	:= { .T., "E-mail enviado com sucesso." }
	Local nTimeout       := 60
	Local nArq           := 0
	Local xRet
	Local oServer, oMessage

	Private cMailConta	:= ""
	Private cMailServer	:= ""
	Private cMailSenha	:= ""
	Default aArquivos 	:= {}
	Default cCopia       := ""

	cMailConta  := Iif(cMailConta  == "", GETMV("MV_RELACNT"), cMailConta)		// Conta utilizada para envio do email
	cMailServer := Iif(cMailServer == "", GETMV("MV_RELSERV"), cMailServer)		// Servidor SMTP
	cMailSenha  := Iif(cMailSenha  == "", GETMV("MV_RELPSW") , cMailSenha)		// Senha da conta de e-mail utilizada para envio

	oMessage:= TMailMessage():New()
	oMessage:Clear()
	oMessage:cDate	 	:= cValToChar( Date() )
	oMessage:cFrom 	:= cMailConta
	oMessage:cTo 	 	:= cPara
	If !Empty(cCopia)
		oMessage:cCC   := cCopia
	EndIf
	oMessage:cSubject	:= cAssunto
	oMessage:cBody 	:= cMensagem

	If Len(aArquivos) > 0
		For nArq := 1 To Len(aArquivos)
			xRet := oMessage:AttachFile( aArquivos[nArq] )
			If xRet < 0
				aRet := { .F., "O arquivo " + aArquivos[nArq] + " não foi anexado!" }
				Return ( aRet )
			EndIf
		Next nArq
	EndIf

	oServer := TMailManager():New()
	oServer:SetUseTLS(SuperGetMv("MV_RELTLS",,.F.))

	xRet := oServer:Init( "", cMailServer, cMailConta, cMailSenha, 0, nPorta )
	If xRet != 0
		aRet := { .F., "O servidor SMTP não foi inicializado: " + oServer:GetErrorString( xRet ) }
		Return ( aRet )
	Endif

	xRet := oServer:SetSMTPTimeout( nTimeout )
	If xRet != 0
		aRet := { .F., "Não foi possível definir tempo limite " + cValToChar( nTimeout ) + " para o servidor SMTP"}
		Return ( aRet )
	Endif

	xRet := oServer:SMTPConnect()
	If xRet <> 0
		aRet := { .F., "Não foi possível conectar ao servidor SMTP: " + oServer:GetErrorString( xRet ) }
		Return ( aRet )
	Endif

	If lMailAuth
		xRet := oServer:SmtpAuth( cMailConta, cMailSenha )
		If xRet <> 0
			aRet := { .F., "Não foi possível autenticar no SMTP server: " + oServer:GetErrorString( xRet ) }
			oServer:SMTPDisconnect()
			Return ( aRet )
		Endif
	Endif

	xRet := oMessage:Send( oServer )
	If xRet <> 0
		aRet := { .F., "Mensagem não enviada: " + oServer:GetErrorString( xRet ) }
	Endif

	xRet := oServer:SMTPDisconnect()

Return ( aRet )

/*/{Protheus.doc} GetChvE1
Encontra a chave utilizada na SE1 para montagem do JSON 

@Author	Michel Sander
@since	13/05/2021 
/*/

User Function GetChvE1(cTabSE1, cTpOper, nRecno , cAreFil, lLimpa )

	Local cChave1 := If(AllTrim(cTabSE1)=="SE1",;
							(cTabSE1)->(E1_FILORIG + Dtos(E1_EMISSAO) + E1_PREFIXO + E1_NUM + E1_TIPO),;// 1a. Chave, sem parcela. Não é chave única no sistema
							(cTabSE1)->(E1_FILORIG + E1_EMISSAO + E1_PREFIXO + E1_NUM + E1_TIPO))// 1a. Chave, sem parcela. Não é chave única no sistema
	Local cChave2 := RetSqlName("SE1") + STRZERO(nRecno, 10) + cAreFil + (cTabSE1)->( E1_CLIENTE + E1_PREFIXO + E1_NUM + E1_TIPO + E1_PARCELA)
	Local cChave3 := (cTabSE1)->(E1_FILORIG + E1_CLIENTE + E1_LOJA + E1_PREFIXO + E1_NUM + E1_TIPO + E1_PARCELA)	//	Atual Chave Única
	Local cChave4 := (cTabSE1)->(E1_FILORIG + E1_PREFIXO + E1_NUM + E1_TIPO + E1_PARCELA) 
	Local cChave  := ""

	If !Empty((cTabSE1)->E1_XCHAVE)
		If At('|', (cTabSE1)->E1_XCHAVE) > 0
			cChave := StrTran((AllTrim((cTabSE1)->E1_XCHAVE)), '|')
		Else
			cChave := (cTabSE1)->E1_XCHAVE
		Endif
	ElseIf U_fLerZ91(cFilAnt, cChave1, cTpOper, "200")
		cChave := cChave1
	ElseIf U_fLerZ91(cFilAnt, cChave2, cTpOper, "200")
		cChave := cChave2
	ElseIf U_fLerZ91(cFilAnt, cChave3, cTpOper, "200")
	   cChave := cChave3
	ElseIf U_fLerZ91(cFilAnt, cChave4, cTpOper, "200")
		cChave := cChave4
	Else
		cChave := cChave3
		If lLimpa
			U_LimpaZ91(cTpOper, cChave1, DtoS(Date()), '401',, cFilAnt)
			U_LimpaZ91(cTpOper, cChave2, DtoS(Date()), '401',, cFilAnt)
			U_LimpaZ91(cTpOper, cChave3, DtoS(Date()), '401',, cFilAnt)
			U_LimpaZ91(cTpOper, cChave4, DtoS(Date()), '401',, cFilAnt)
			cTpExcl := AllTrim(GetAdvFVal("Z96","Z96_DTFECH",xFilial("Z96",cFilZ90)+cTpOper,1,""))
			If !Empty(cTpExcl)
				U_LimpaZ91(cTpExcl,cChave1, DtoS(Date()),'401',,cFilZ90)
				U_LimpaZ91(cTpExcl,cChave2, DtoS(Date()),'401',,cFilZ90)
				U_LimpaZ91(cTpExcl,cChave3, DtoS(Date()),'401',,cFilZ90)
				U_LimpaZ91(cTpExcl,cChave4, DtoS(Date()),'401',,cFilZ90)
			EndIf
		EndIf
	EndIf
	cChave := RTrim(cChave)

Return ( cChave )

/*/{Protheus.doc} GetChvE2
Encontra a chave utilizada na SE2 para montagem do JSON 

@Author	Michel Sander
@since	13/05/2021 
/*/

User Function GetChvE2(cChvE21, cChvE22, cTpOper, cAreFil)

	LOCAL cChvBusca := ""

	If SE2->(FieldPos("E2_XCHAVE")) > 0
	   If !Empty(SE2->E2_XCHAVE)
		   cChvBusca := SE2->E2_XCHAVE
		EndIf 
	ElseIf U_fLerZ91(cAreFil, cChvE21, cTpOper, "200")
		cChvBusca := cChvE21
	ElseIf U_fLerZ91(cAreFil, cChvE22, cTpOper, "200")
		cChvBusca := cChvE22
	Else
		cChvBusca := cChvE21
	EndIf
	
Return ( cChvBusca )

/*/{Protheus.doc} SeqChvZ91
Encontra a proxima sequencia de chave para o Log de registros do Arena

@Author	Michel Sander
@since	13/05/2021
/*/

User Function SeqChvZ91(_cFilial, _cTipo, _cChave, lProximo)

	Local aArea			:= GetArea()
	Local aAreaZ91		:= Z91->(GetArea())
	Local aAreas      := { aArea, aAreaZ91 }
	Local cSequencia	:= ""
	Local nTamSeq     := TamSX3("Z91_SEQCHV")[1]
	Local cAliasZ91   := GetNextAlias()

	DEFAULT lProximo := .F.

	BEGINSQL Alias cAliasZ91
		
		SELECT MAX(Z91_SEQCHV) MAXSEQ FROM %Table:Z91% Z91 WHERE
				Z91.Z91_FILIAL = %Exp:_cFilial% AND
				Z91.Z91_TPOPER = %Exp:_cTipo% AND 
				Z91.Z91_CHAVE  = %Exp:_cChave% AND
				Z91.%NotDel%
	ENDSQL 

	cSequencia := (cAliasZ91)->MAXSEQ
	If Empty(cSequencia) 
		cSequencia := Repl("0",nTamSeq)
	EndIf 

	If lProximo 
		cSequencia := Soma1(cSequencia)
	EndIf 

	(cAliasZ91)->(dbCloseArea())
	AEval( aAreas, { |x| RestArea(x) } )

Return ( cSequencia )

/*/
Função para procurar o histórico da chave de inclusão e cancelamento na tabela de LOG do Arena
@project 	Habibs
@author 	   Michel Sander
@since 		19/08/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function fHistZ91(cxFilial, cxChave, cxOK, cxTpIncl, cxTpExcl, cxSeqChv)

	Local lPermite := .T.
	Local cQuery   := GetNextAlias()
	Local aArea    := GetArea()
	Local cIn      := ""
	Local cxTpUso  := ""
	Local nIncl    := 0
	Local nExcl    := 0 

	DEFAULT cxSeqChv := ""	

   cIn 	  := FormatIn(AllTrim(cxTpIncl)+","+AllTrim(cxTpExcl),",") 
	cxTpUso := "%Z91.Z91_TPOPER IN "+cIn+"%"

	If !Empty(cxSeqChv)

		BEGINSQL Alias cQuery

			SELECT Z91_FILIAL, Z91_TPOPER, Z91_STATUS FROM %Table:Z91% Z91
						WHERE Z91_FILIAL = %Exp:cxFilial%
						AND Z91_CHAVE = %Exp:cxChave%
						AND Z91_STATUS = %Exp:cxOK%
						AND Z91_SEQCHV = %Exp:cxSeqChv%
						AND %Exp:cxTpUso%
						AND Z91.%NotDel%
						ORDER BY Z91.Z91_FILIAL, Z91.Z91_TPOPER
		ENDSQL

	Else
		
		BEGINSQL Alias cQuery

			SELECT Z91_FILIAL, Z91_TPOPER, Z91_STATUS FROM %Table:Z91% Z91
						WHERE Z91_FILIAL = %Exp:cxFilial%
						AND Z91_CHAVE = %Exp:cxChave%
						AND Z91_STATUS = %Exp:cxOK%
						AND %Exp:cxTpUso%
						AND Z91.%NotDel%
						ORDER BY Z91.Z91_FILIAL, Z91.R_E_C_N_O_
		ENDSQL

	EndIf 

	If (cQuery)->(Eof())
	   If !Empty(cxSeqChv)
	   	lPermite := .F.
		Else
		   lPermite := .T.
		EndIf 
	Else
	   While (cQuery)->(!EOf())
		   If !Empty(cxSeqChv)
				If AllTrim((cQuery)->Z91_TPOPER) == AllTrim(cxTpIncl) 
					nIncl := 1
				ElseIf AllTrim((cQuery)->Z91_TPOPER) == AllTrim(cxTpExcl) 
					nExcl := 1
				EndIf 
			Else 
				If AllTrim((cQuery)->Z91_TPOPER) == AllTrim(cxTpIncl) 
					nIncl := 1
				ElseIf AllTrim((cQuery)->Z91_TPOPER) == AllTrim(cxTpExcl) 
					nIncl := 0
				EndIf 
			EndIf 
			(cQuery)->(dbSkip())
		End
		If !Empty(cxSeqChv) 
			If (nIncl-nExcl) == 0
				lPermite := .F.
			EndIf 
		Else
			If nIncl == 0
				lPermite := .F.
			EndIf 
		EndIf 
	Endif

	(cQuery)->(dbCloseArea())
	RestArea(aArea)

Return ( lPermite )

/*/ { fVerTimeOut() }
Função para eliminar os erros de timed-out no Arena antes do reenvio de registros 

@project 	Habibs
@author 	   Michel Sander
@since 		19/08/2021
@version 	P12.1.17
@type 		User Function
@history 	
/*/ 

User Function fVerTimeOut(cFilUso, cTipoUso, cChaveUso, cStatUso)

LOCAL cZ91Alias := GetNextAlias()

BEGINSQL Alias cZ91Alias

	SELECT R_E_C_N_O_ RECNOZ91 FROM %Table:Z91% Z91
				WHERE Z91_FILIAL = %Exp:cFilUso%
				AND Z91_CHAVE = %Exp:cChaveUso%
				AND Z91_STATUS = %Exp:cStatUso%
				AND Z91_TPOPER = %Exp:cTipoUso%
				AND Z91_MENSAG LIKE '%timed out%'
				AND Z91.%NotDel%

ENDSQL

While (cZ91Alias)->(!Eof())
   SET DELETED OFF
   Z91->(dbGoto((cZ91Alias)->RECNOZ91))
	Reclock("Z91",.F.)
	Z91->(dbDelete())
	Z91->(MsUnlock())
	SET DELETED ON
	(cZ91Alias)->(dbSkip())
End

(cZ91Alias)->(DbCloseArea())

Return
