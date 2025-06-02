/****** Object:  StoredProcedure [dbo].[P_F_PRC_PRICE_LIST]    Script Date: 19/05/2025 10:43:52 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/**
Pripravil:Andreja Lapajne: 24.5.2023
dopolnila Urška Drabik Eržen, 6.1.2025
**/

CREATE OR ALTER PROCEDURE  [dbo].[P_F_PRC_PRICE_LIST] AS
BEGIN
 
    DECLARE @ID uniqueidentifier
    DECLARE @ERROR_MSG VARCHAR(4000)
    DECLARE @USER varchar(50);
    DECLARE @PROCEDURE_NAME varchar(250);
 
    SET @ID=NEWID()
    set @USER = (SELECT SUSER_NAME());
    set @PROCEDURE_NAME = '[dbo].[P_F_PRC_PRICE_LIST]';--OBJECT_NAME(@@PROCID) ne deluje
 
    BEGIN TRY
        EXEC [INFO_Warehouse].[dbo].[etlSTART]  
            @ID=@ID,
            @TYPE='GOLD',
            @DESCRIPTION='prod',
            @USER = @USER,
            @DATASOURCE_NAME='SILVER - Navision',
            @SOURCE_TABLE = 'f_ium_item_unit_of_measure,stg_f_prc_price_list,d_cus_customer,f_iuc_item_unit_of_measure_converter',
            @TARGET_TABLE = 'f_prc_price_list',
            @PROCEDURE_NAME = @PROCEDURE_NAME

DROP TABLE IF EXISTS PRC_SALES_UNIT_TEMP
DROP TABLE IF EXISTS PRC_KARTON_TEMP
DROP TABLE IF EXISTS PRC_BASE_UNIT_TEMP


------------------------ IZRAČUN CENE NA OSNOVNO MERSKO ENOTO
SELECT
	[$systemId],
	[Unit of Measure Code]  [PRC Unit of Measure Code], -- merska enota iz cenika
	[Unit Price] [PRC Unit Price], -- cena na mersko enoto iz cenika
	M.[ITM Base Unit of Measure], -- osnovna merska enota
	([Base Quantity] / [IUM Qty_ per Unit of Measure]) * prc.[Unit Price] AS [PRC Price per Base Unit]
		-- base quantity je iz IUC converterja, poda količino glede na osnovno mersko enoto artikla (torej 1)
		-- IUM qt per unit of measure pove količino glede na mersko enoto iz cenika (ker je v outer apply IUM vezan tudi s kodo merske enote)(sidenote: govorila z andrejo, ne ve točno zakaj je tukaj outer apply, lahko bi bil join)
		-- kar je v () da količino v osnovni merski enoti, potem to pomnožimo z Unit Price in dobimo ceno za eno osnovno mersko enoto
INTO PRC_BASE_UNIT_TEMP
FROM
	[SILVER_Warehouse].[dbo].[stg_f_prc_price_list] prc
	outer apply (select [IUM Qty_ per Unit of Measure] from [GOLD_Warehouse].[dbo].[f_ium_item_unit_of_measure] ium where  prc.[Company] + prc.[Asset No_] = ium.[ITM Key] and  [IUM Code] = [Unit of Measure Code]) a
	left join (SELECT ITM.[ITM No_],[ITM Base Unit of Measure] ,ISNULL(
		  CASE WHEN [ITM Base Unit of Measure] = 'KG' THEN iuc.[KG]
			   WHEN [ITM Base Unit of Measure] = 'KARTON' THEN iuc.[KARTON]
			   WHEN [ITM Base Unit of Measure] = 'KOS' THEN iuc.[KOS]
			   WHEN [ITM Base Unit of Measure] = 'PCS' THEN iuc.[PCS]
			   WHEN [ITM Base Unit of Measure] = N'L' THEN iuc.[L]
			   WHEN [ITM Base Unit of Measure] = 'GJ' THEN iuc.[GJ]
			   WHEN [ITM Base Unit of Measure] = '#' THEN iuc.[#]
			   WHEN [ITM Base Unit of Measure] = 'CM' THEN iuc.[CM]
			   WHEN [ITM Base Unit of Measure] = 'M2' THEN iuc.[M2]
			   WHEN [ITM Base Unit of Measure] = 'MP' THEN iuc.[MP]
			   WHEN [ITM Base Unit of Measure] = 'L' THEN iuc.[L]
			   WHEN [ITM Base Unit of Measure] = 'M3' THEN iuc.[M3]
			   WHEN [ITM Base Unit of Measure] = 'ZAV' THEN iuc.[ZAV]
			   WHEN [ITM Base Unit of Measure] = 'URA' THEN iuc.[URA]
			   WHEN [ITM Base Unit of Measure] = 'KOM' THEN iuc.[KOM]
			   WHEN [ITM Base Unit of Measure] = 'MG/KG' THEN iuc.[MG/KG]
			   WHEN [ITM Base Unit of Measure] = 'PALETA' THEN iuc.[PALETA]
			   WHEN [ITM Base Unit of Measure] = 'ML' THEN iuc.[ML]
			   WHEN [ITM Base Unit of Measure] = 'M' THEN iuc.[M]
			   WHEN [ITM Base Unit of Measure] = 'TK' THEN iuc.[TK]
			   ELSE null END,null) [Base Quantity]	  FROM [GOLD_Warehouse].[dbo].[d_itm_item] ITM
		left join [GOLD_Warehouse].[dbo].[f_iuc_item_unit_of_measure_converter] iuc on ITM.[ITM Key] = iuc.[ITM Key]
			) M oN  prc.[Asset No_]  = M.[ITM No_]  


------------------------ IZRAČUN CENE NA PRODAJNO MERSKO ENOTO
SELECT
	[$systemId],
	[Unit of Measure Code]  [PRC Unit of Measure Code], -- merska enota iz cenika
	[Unit Price] [PRC Unit Price], -- cena na mersko enoto iz cenika
	M.[ITM Sales Unit of Measure], -- prodajna merska enota
	([Sales Quantity] / [IUM Qty_ per Unit of Measure]) * prc.[Unit Price] AS [PRC Price per Sales Unit]
		-- sales quantity je iz IUC converterja, poda količino glede na prodajno mersko enoto artikla (torej 1)
		-- IUM qt per unit of measure pove količino glede na mersko enoto iz cenika (ker je v outer apply IUM vezan tudi s kodo merske enote)(sidenote: govorila z andrejo, ne ve točno zakaj je tukaj outer apply, lahko bi bil join)
		-- kar je v () da količino v prodajni merski enoti, potem to pomnožimo z Unit Price in dobimo ceno za eno prodajno mersko enoto
INTO PRC_SALES_UNIT_TEMP
FROM
	[SILVER_Warehouse].[dbo].[stg_f_prc_price_list] prc
	outer apply (select [IUM Qty_ per Unit of Measure] from [GOLD_Warehouse].[dbo].[f_ium_item_unit_of_measure] ium where  prc.[Company] + prc.[Asset No_] = ium.[ITM Key] and  [IUM Code] = [Unit of Measure Code]) a
	left join (SELECT ITM.[ITM No_],[ITM Sales Unit of Measure] ,ISNULL(
		  CASE WHEN [ITM Sales Unit of Measure] = 'KG' THEN iuc.[KG]
			   WHEN [ITM Sales Unit of Measure] = 'KARTON' THEN iuc.[KARTON]
			   WHEN [ITM Sales Unit of Measure] = 'KOS' THEN iuc.[KOS]
			   WHEN [ITM Sales Unit of Measure] = 'PCS' THEN iuc.[PCS]
			   WHEN [ITM Sales Unit of Measure] = N'L' THEN iuc.[L]
			   WHEN [ITM Sales Unit of Measure] = 'GJ' THEN iuc.[GJ]
			   WHEN [ITM Sales Unit of Measure] = '#' THEN iuc.[#]
			   WHEN [ITM Sales Unit of Measure] = 'CM' THEN iuc.[CM]
			   WHEN [ITM Sales Unit of Measure] = 'M2' THEN iuc.[M2]
			   WHEN [ITM Sales Unit of Measure] = 'MP' THEN iuc.[MP]
			   WHEN [ITM Sales Unit of Measure] = 'L' THEN iuc.[L]
			   WHEN [ITM Sales Unit of Measure] = 'M3' THEN iuc.[M3]
			   WHEN [ITM Sales Unit of Measure] = 'ZAV' THEN iuc.[ZAV]
			   WHEN [ITM Sales Unit of Measure] = 'URA' THEN iuc.[URA]
			   WHEN [ITM Sales Unit of Measure] = 'KOM' THEN iuc.[KOM]
			   WHEN [ITM Sales Unit of Measure] = 'MG/KG' THEN iuc.[MG/KG]
			   WHEN [ITM Sales Unit of Measure] = 'PALETA' THEN iuc.[PALETA]
			   WHEN [ITM Sales Unit of Measure] = 'ML' THEN iuc.[ML]
			   WHEN [ITM Sales Unit of Measure] = 'M' THEN iuc.[M]
			   WHEN [ITM Sales Unit of Measure] = 'TK' THEN iuc.[TK]
			   ELSE null END,null) [Sales Quantity]	  FROM [GOLD_Warehouse].[dbo].[d_itm_item] ITM
		left join [GOLD_Warehouse].[dbo].[f_iuc_item_unit_of_measure_converter] iuc on ITM.[ITM Key] = iuc.[ITM Key]
			) M oN  prc.[Asset No_]  = M.[ITM No_]  

	

------------------------------ IZRAČUN CENE NA KARTON
SELECT
	prc.[$systemId],
	CASE 
		WHEN prc.[Unit of Measure Code] = 'KARTON' THEN [Unit Price] -- merska enota iz cenika
		WHEN itm.[ITM Sales Unit of Measure] = 'KARTON' THEN su.[PRC Price per Sales Unit] -- prodajna merska enota
		ELSE (1 / ium.[IUM Qty_ per Unit of Measure]) * su.[PRC Price per Sales Unit]
	END AS [PRC Price per KARTON]
INTO PRC_KARTON_TEMP
FROM
	[SILVER_Warehouse].[dbo].[stg_f_prc_price_list] prc
	left join [GOLD_Warehouse].[dbo].[d_itm_item] itm on prc.[Asset No_] = itm.[ITM No_]
	left join [GOLD_Warehouse].[dbo].[f_ium_item_unit_of_measure] ium on prc.[Company] + prc.[Asset No_] = ium.[ITM Key] and  [IUM Code] = itm.[ITM Sales Unit of Measure]
	left join PRC_SALES_UNIT_TEMP su on su.[$systemId] = prc.[$systemId]



 
delete from f_prc_price_list
 
insert into f_prc_price_list
SELECT [Company]  AS [NAV Company]
,[Price List Code] [PRC Price List Code]
,[Line No_] [PRC Line No_]
,[Source Type] [PRC Source Type]
,[Source No_] [PRC Source No_]
,[Source ID] [PRC Source ID]
,[Asset Type] [PRC Asset Type]
,[Asset No_] [PRC Asset No_]
,cast([Starting Date] as date) [PRC Starting Date]
,Case when [Ending Date] = '1753-01-01' then '9999-12-31'
 else cast([Ending Date]  as date) end [PRC Ending Date]
,[Minimum Quantity] [PRC Minimum Quantity]
,[Unit of Measure Code]  [PRC Unit of Measure Code]
,[Amount Type] [PRC Amount Type]
,[Unit Price] [PRC Unit Price] -- unit ki je v ceniku, kar ni vedno isto kot sales unit!
,bu.[PRC Price per Base Unit]
,su.[PRC Price per Sales Unit]
,k.[PRC Price per KARTON]
,[Cost Factor]  [PRC Cost Factor]
,[Unit Cost] [PRC Unit Cost]
,[Line Discount _] [PRC Line Discount _]
,[Allow Line Disc_] [PRC Allow Line Disc_]
,[Allow Invoice Disc_] [PRC Allow Invoice Disc_] 
,[Price Includes VAT] [PRC Price Includes VAT] 
,[VAT Bus_ Posting Gr_ _Price_] [PRC VAT Bus_ Posting Gr_ (Price)] 
,[Line Amount] [PRC Line Amount]
,[Price Type] [PRC Price Type] 
,[Description Header] [PRC Description Header] 
,[Description] [PRC Description]
,[Status] [PRC Status]
,[Direct Unit Cost] [PRC Direct Unit Cost]
,[Source Group] [PRC Source Group] 
,prc.[$systemId] [PRC $systemId]
,[$systemCreatedBy] [PRC $systemCreatedBy] 
,[Company] + [Product No_]  [ITM Key]
,[Assign-to No_] [PRC Assign-to No_]
,[Unit of Measure Code Lookup] [PRC Unit of Measure Code Lookup] 
,[Code] [PRC Code] 
,[Price Type Header] [PRC Price Type Header] 
,[No_ Series] [PRC No_ Series]
,[Filter Source No_] [PRC Filter Source No_]
,[Allow Updating Defaults] [PRC Allow Updating Defaults]
, case when [IUM Qty_ per Unit of Measure] is null then 1
else [IUM Qty_ per Unit of Measure] end as [PRC Qty_ per KARTON]
,CASE WHEN M.[ITM Sales Unit of Measure] = [Unit of Measure Code]  THEN 1
					ELSE [Sales Quantity] END [PRC Sales Base Quantity]
,case when [CUS Customer Group] is null then '01 NERAZPOREJENO'
else [CUS Customer Group] end AS [PRC Cus Group]
, IIF([Ending Date] = '1753-01-01' OR [Ending Date] >= GETDATE(), 1,0) [PRC Active Status]
FROM [SILVER_Warehouse].[dbo].[stg_f_prc_price_list] prc
  outer apply (select [IUM Qty_ per Unit of Measure] from [GOLD_Warehouse].[dbo].[f_ium_item_unit_of_measure] ium where  prc.[Company] + prc.[Asset No_] = ium.[ITM Key] and  [IUM Code] = [Unit of Measure Code]) a
  left join (SELECT ITM.[ITM No_],[ITM Sales Unit of Measure] ,ISNULL(
		  CASE WHEN [ITM Sales Unit of Measure] = 'KG' THEN iuc.[KG]
			   WHEN [ITM Sales Unit of Measure] = 'KARTON' THEN iuc.[KARTON]
			   WHEN [ITM Sales Unit of Measure] = 'KOS' THEN iuc.[KOS]
			   WHEN [ITM Sales Unit of Measure] = 'PCS' THEN iuc.[PCS]
			   WHEN [ITM Sales Unit of Measure] = N'L' THEN iuc.[L]
			   WHEN [ITM Sales Unit of Measure] = 'GJ' THEN iuc.[GJ]
			   WHEN [ITM Sales Unit of Measure] = '#' THEN iuc.[#]
			   WHEN [ITM Sales Unit of Measure] = 'CM' THEN iuc.[CM]
			   WHEN [ITM Sales Unit of Measure] = 'M2' THEN iuc.[M2]
			   WHEN [ITM Sales Unit of Measure] = 'MP' THEN iuc.[MP]
			   WHEN [ITM Sales Unit of Measure] = 'L' THEN iuc.[L]
			   WHEN [ITM Sales Unit of Measure] = 'M3' THEN iuc.[M3]
			   WHEN [ITM Sales Unit of Measure] = 'ZAV' THEN iuc.[ZAV]
			   WHEN [ITM Sales Unit of Measure] = 'URA' THEN iuc.[URA]
			   WHEN [ITM Sales Unit of Measure] = 'KOM' THEN iuc.[KOM]
			   WHEN [ITM Sales Unit of Measure] = 'MG/KG' THEN iuc.[MG/KG]
			   WHEN [ITM Sales Unit of Measure] = 'PALETA' THEN iuc.[PALETA]
			   WHEN [ITM Sales Unit of Measure] = 'ML' THEN iuc.[ML]
			   WHEN [ITM Sales Unit of Measure] = 'M' THEN iuc.[M]
			   WHEN [ITM Sales Unit of Measure] = 'TK' THEN iuc.[TK]
			   ELSE null END,null) [Sales Quantity]	  FROM [GOLD_Warehouse].[dbo].[d_itm_item] ITM
		left join [GOLD_Warehouse].[dbo].[f_iuc_item_unit_of_measure_converter] iuc on ITM.[ITM Key] = iuc.[ITM Key]
		
										) M oN  prc.[Asset No_]  = M.[ITM No_]  
	    left join [GOLD_Warehouse].[dbo].[d_cus_customer] b on [CUS No_] = [Source No_]
		left join PRC_KARTON_TEMP k on k.[$systemId] = prc.[$systemId]
		left join PRC_SALES_UNIT_TEMP su on prc.[$systemId] = su.[$systemId]
		left join PRC_BASE_UNIT_TEMP bu on prc.[$systemId] = bu.[$systemId]

DROP TABLE IF EXISTS PRC_SALES_UNIT_TEMP
DROP TABLE IF EXISTS PRC_KARTON_TEMP
DROP TABLE IF EXISTS PRC_BASE_UNIT_TEMP


      EXEC [INFO_Warehouse].[dbo].[etlSTOP] @ID=@ID
    END TRY
    BEGIN CATCH
        SET @ERROR_MSG=ERROR_MESSAGE()
        EXEC [INFO_Warehouse].[dbo].[etlERROR] @ID=@ID, @ERROR=@ERROR_MSG;
    END CATCH;
END
GO


