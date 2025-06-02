/****** Object:  StoredProcedure [dbo].[P_F_PAK_POSTCALCULATION_ANALYSIS]    Script Date: 19/05/2025 10:42:32 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO






/**
Pripravil:Andreja Lapajne: 15.01.2024
**/

CREATE OR ALTER PROCEDURE  [dbo].[P_F_PAK_POSTCALCULATION_ANALYSIS] AS
BEGIN


DECLARE @Tran nvarchar(50)
SET @Tran = 'Tran1'
	DECLARE @ID uniqueidentifier 
	DECLARE @ERROR_MSG VARCHAR(4000) 
	DECLARE @USER varchar(50);
	DECLARE @PROCEDURE_NAME varchar(250);

	SET @ID=NEWID() 
	set @USER = (SELECT SUSER_NAME());
	set @PROCEDURE_NAME = 'P_F_PAK_POSTCALCULATION_ANALYSIS';

    BEGIN TRY
		EXEC [INFO_Warehouse].[dbo].[etlSTART]  
			@ID=@ID,
			@TYPE='GOLD',
			@DESCRIPTION='prod', 
			@USER = @USER,
			@DATASOURCE_NAME='GOLD - Navision',
			@SOURCE_TABLE = 'MAIN stg_f_pak_postcalculation_analysis', 
			@TARGET_TABLE = 'f_pak_postcalculation_analysis',
			@PROCEDURE_NAME = @PROCEDURE_NAME


truncate  table f_pak_postcalculation_analysis
DROP TABLE IF EXISTS PAK_PRODUCTION_GOLD_TEMP
DROP TABLE IF EXISTS PAK_HOURSS_GOLD_TEMP
DROP TABLE IF EXISTS PAK_ROUTING_HOURS_GOLD_TEMP
DROP TABLE IF EXISTS PAK_WORK_CENTER_GOLD_TEMP
DROP TABLE IF EXISTS PAK_MATERIAL_GOLD_TEMP
DROP TABLE IF EXISTS PAK_COST_AND_PERCENTAGE_GOLD_TEMP
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_4
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_3
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_2
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_1
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP_3
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP_2
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP_1
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP
DROP TABLE IF EXISTS PAK_PRICE_GOLD_TEMP_1
DROP TABLE IF EXISTS PAK_PRICE_GOLD_TEMP
DROP TABLE IF EXISTS PAK_DOWNTIME_GOLD_TEMP
DROP TABLE IF EXISTS PAK_CONVERTOR_GOLD_TEMP
DROP TABLE IF EXISTS PAK_FINAL_GOLD_TEMP
DROP TABLE IF EXISTS PAK_REALISATION_GOLD_TEMP

		SELECT 
		    t.[ITM Key],
		    ROUND(1.0 / M.[Sales Quantity], 0) AS [TK/MP Convertor]
			INTO PAK_CONVERTOR_GOLD_TEMP
		FROM 
		    d_itm_item t
		LEFT JOIN (
		    SELECT 
		        ITM.[ITM No_],
		        [ITM Sales Unit of Measure],
		        ISNULL(
		            CASE 
		                WHEN [ITM Sales Unit of Measure] = 'KG' THEN iuc.[KG]
		                WHEN [ITM Sales Unit of Measure] = 'KARTON' THEN iuc.[KARTON]
		                WHEN [ITM Sales Unit of Measure] = 'KOS' THEN iuc.[KOS]
		                WHEN [ITM Sales Unit of Measure] = 'PCS' THEN iuc.[PCS]
		                WHEN [ITM Sales Unit of Measure] = N'L' THEN iuc.[L]
		                WHEN [ITM Sales Unit of Measure] = 'GJ' THEN iuc.[GJ]
		                WHEN [ITM Sales Unit of Measure] = '#' THEN iuc.[#]
		                WHEN [ITM Sales Unit of Measure] = 'CM' THEN iuc.[CM]
		                WHEN [ITM Sales Unit of Measure] = 'M2' THEN iuc.[M2]
		                WHEN [ITM Sales Unit of Measure] = 'MP' THEN iuc.[MP]
		                WHEN [ITM Sales Unit of Measure] = 'CL' THEN iuc.[CL]
		                WHEN [ITM Sales Unit of Measure] = 'M3' THEN iuc.[M3]
		                WHEN [ITM Sales Unit of Measure] = 'ZAV' THEN iuc.[ZAV]
		                WHEN [ITM Sales Unit of Measure] = 'URA' THEN iuc.[URA]
		                WHEN [ITM Sales Unit of Measure] = 'KOM' THEN iuc.[KOM]
		                WHEN [ITM Sales Unit of Measure] = 'MG/KG' THEN iuc.[MG/KG]
		                WHEN [ITM Sales Unit of Measure] = 'PALETA' THEN iuc.[PALETA]
		                WHEN [ITM Sales Unit of Measure] = 'ML' THEN iuc.[ML]
		                WHEN [ITM Sales Unit of Measure] = 'M' THEN iuc.[M]
		                WHEN [ITM Sales Unit of Measure] = 'TK' THEN iuc.[TK]
		                ELSE NULL 
		            END, 
		            NULL
		        ) AS [Sales Quantity]
		    FROM 
		        [GOLD_Warehouse].[dbo].[d_itm_item] ITM
		    LEFT JOIN 
		        [GOLD_Warehouse].[dbo].[f_iuc_item_unit_of_measure_converter] iuc 
		    ON 
		        ITM.[ITM Key] = iuc.[ITM Key] 
		) M 
		ON  
		    t.[ITM No_] = M.[ITM No_] 
	
------------------------------------------------------

    SELECT
        [PCF Prod_ Order No_] AS [Prod_ Order No_],
		[PCF No_] AS [No_],
		[PCF Type] AS [Type],
		[PCF Task Type] AS [Task Type],
        SUM([PCF Posted Ammount]) AS [Posted Production Value],
        SUM([PCF Planned Ammount])  AS [Planned Production Value],
		SUM([PCF Order Ammount]) AS [Order Ammount]
	INTO PAK_PRODUCTION_GOLD_TEMP
    FROM [GOLD_Warehouse].[dbo].[f_pcf_post_calculation_buffer] buf1 
    GROUP BY [PCF Prod_ Order No_], [PCF No_], [PCF Type], [PCF Task Type]

------------------------------------------------------

    SELECT
        [PCF Prod_ Order No_] AS [Prod_ Order No_],
		[PCF No_] AS [No_],
		[PCF Type] AS [Type],
		[PCF Task Type] AS [Task Type],
        SUM([PCF Posted Ammount]) AS [Posted Hours Amount],
        SUM([PCF Posted Qty_]) AS [Posted Hours Quantity],
        SUM([PCF Planned Ammount]) AS [Planned Hours Amount],
        SUM([PCF Planned Qty_]) AS [Planned Hours Quantity]
   INTO PAK_HOURSS_GOLD_TEMP
   FROM [GOLD_Warehouse].[dbo].[f_pcf_post_calculation_buffer] buf1
    WHERE [PCF Type] = 3
    GROUP BY [PCF Prod_ Order No_], [PCF No_], [PCF Type], [PCF Task Type]


------------------------------------------------------

	SELECT
		[PCF Prod_ Order No_] [Prod_ Order No_],
		[PCF No_] AS [No_],
		[PCF Type] AS [Type],
		[PCF Task Type] AS [Task Type],
		SUM([PCF Planned Qty_]) [Manufacturing Planned Quantity] , 
		SUM([PCF Posted Qty_])   [Manufacturing Posted Quantity]
	INTO PAK_ROUTING_HOURS_GOLD_TEMP
	FROM 
		[GOLD_Warehouse].[dbo].[f_pcf_post_calculation_buffer] buf
    WHERE  
		[PCF No_] = 'PAK' 
		AND [PCF Type]= 2 
		AND [PCF Task Type] = 0
    GROUP BY 
		[PCF Prod_ Order No_], [PCF No_], [PCF Type], [PCF Task Type]


------------------------------------------------------

   SELECT
        [PCF Prod_ Order No_] AS [Prod_ Order No_],
		[PCF No_] AS [No_],
		[PCF Type] AS [Type],
		[PCF Task Type] AS [Task Type],
        SUM([PCF Posted Ammount]) AS [Posted Work Center Amount],
        SUM([PCF Planned Ammount]) AS [Planned Work Center Amount]
    INTO PAK_WORK_CENTER_GOLD_TEMP
	FROM [GOLD_Warehouse].[dbo].[f_pcf_post_calculation_buffer] buf1
    WHERE [PCF Type] = 2
    GROUP BY [PCF Prod_ Order No_], [PCF No_], [PCF Type], [PCF Task Type]


------------------------------------------------------

  SELECT
        [PCF Prod_ Order No_] AS [Prod_ Order No_],
		[PCF No_] AS [No_],
		[PCF Type] AS [Type],
		[PCF Task Type] AS [Task Type],
        SUM([PCF Posted Ammount]) AS [Posted Material Cost Amount],
        SUM([PCF Planned Ammount]) AS [Planned Material Cost Amount]
    INTO PAK_MATERIAL_GOLD_TEMP
	FROM [GOLD_Warehouse].[dbo].[f_pcf_post_calculation_buffer] buf1
    WHERE [PCF Type] = 1 AND [PCF Task Type] = 0
    GROUP BY [PCF Prod_ Order No_], [PCF No_], [PCF Type], [PCF Task Type]

-------------- Cene ----------------------------------------
-- Modified by Urska Drabik Erzen, 21.3.2025

select
	prd.[PRD Prod_ Order No_],
	prd.[ITM Key],
	prd.[PRD Starting Date],
	prc.[PRC Price per KARTON],
	prc.[PRC Starting Date],
	prc.[PRC Ending Date]
into PAK_PRICE_GOLD_TEMP_1
from [dbo].[f_prd_production_order_line] prd
	left join f_prc_price_list prc on prd.[ITM Key] = prc.[ITM Key]
where 
	prd.[PRD Line No_] = 10000
	and [PRC Price List Code] not in ('S01092', -- incom donacije
							'S01093', -- incom sponzorstvo
							'S00983') -- maloprodajni cenik
	AND [PRC Unit Price] <> 0 -- to avoid doing an AVG([PRC Price per KARTON]) with rows with 0 in them - bc it does (7.9+0)/2
	AND [PRC Status] <> 2 -- to je status celotnega cenika. 2 = neaktiven, 1 = aktiven, 0 = osnutek. Ziga javil, da mora biti vkljucen tudi osnutek
	AND CONCAT(YEAR([PRC Starting Date]), DATEPART(QUARTER, [PRC Starting Date])) <= CONCAT(YEAR(prd.[PRD Starting Date]), DATEPART(QUARTER, prd.[PRD Starting Date])) -- starting year_quarter of price has to be lower than year_quarter of start date of dni
	AND CONCAT(YEAR([PRC Ending Date]), DATEPART(QUARTER, [PRC Ending Date])) >= CONCAT(YEAR(prd.[PRD Starting Date]), DATEPART(QUARTER, prd.[PRD Starting Date])) -- ending year_quarter of price has to be higher than year_quarter of start date of dni
	AND DATEDIFF(DAY, [PRC Starting Date], [PRC Ending Date]) > 1 -- da ne vkljucimo cen, ki veljajo samo en dan

select
	[PRD Prod_ Order No_],
	[ITM Key],
	AVG([PRC Price per KARTON]) as [PRC Price per KARTON]
into PAK_PRICE_GOLD_TEMP
from PAK_PRICE_GOLD_TEMP_1
group by 
	[PRD Prod_ Order No_],
	[ITM Key]


-------------- KALKULACIJE ----------------------------------------
-- Modified by Urska Drabik Erzen, 21.3.2025

-------- Zadnja kalkulacija v obdobju --------

select 
	[PRD Prod_ Order No_],
	prd.[ITM Key],
	[PRD Starting Date],
	[CSH On Date],
	[CSH Document Date Time],
	[CSH Document No_]
into PAK_CSH_CONNECTION_TEMP_4
from [GOLD_Warehouse].[dbo].[f_prd_production_order_line] prd
	inner join 
		(
		select	
			[CSH Company],
			[CSH Item No_],
			[CSH On Date],
			[CSH Document Date Time],
			[CSH Document No_]
		from [GOLD_Warehouse].[dbo].[f_csh_cost_share]
		where [CSH Indentation] = 0 and [CSH Is Leaf] = 0
		--and [CSH Item No_] = '16003008'
		) a on prd.[ITM Key] = a.[CSH Company] + a.[CSH Item No_]
where 
	CONCAT(YEAR(a.[CSH On Date]), DATEPART(QUARTER, a.[CSH On Date])) 
	= 
	CONCAT(YEAR(prd.[PRD Starting Date]), DATEPART(QUARTER, prd.[PRD Starting Date]))
	and prd.[PRD Line No_] = 10000

select 
	*, 
	RANK() OVER(PARTITION BY [PRD Prod_ Order No_], [ITM Key] ORDER BY [CSH On Date] desc, [CSH Document Date Time] desc) as rnk
into PAK_CSH_CONNECTION_TEMP_3
from PAK_CSH_CONNECTION_TEMP_4


select *
into PAK_CSH_CONNECTION_TEMP_2
from PAK_CSH_CONNECTION_TEMP_3
where rnk = 1


select
	f.*,
	[CSH Unit Cost] AS [CSH Unit Cost in Quarter],
	[CSH Total Cost] AS [CSH Total Cost in Quarter]
into PAK_CSH_CONNECTION_TEMP_1
from PAK_CSH_CONNECTION_TEMP_2 f
	left join [GOLD_Warehouse].[dbo].[f_csh_cost_share] c on f.[CSH Document No_] = c.[CSH Document No_]
where [CSH Indentation] = 0 and [CSH Is Leaf] = 0


-------- Zadnja kalkulacija in general --------

select	
	[CSH Company],
	[CSH Item No_],
	[CSH On Date],
	[CSH Document Date Time],
	[CSH Document No_]
into PAK_CALCULATION_GOLD_TEMP_3
from [GOLD_Warehouse].[dbo].[f_csh_cost_share] f_csh
where [CSH Indentation] = 0 and [CSH Is Leaf] = 0
AND [CSH On Date] = (select max([CSH On Date]) from [GOLD_Warehouse].[dbo].[f_csh_cost_share] on_date_csh
					where [CSH Indentation] = 0 and [CSH Is Leaf] = 0
						 and on_date_csh.[CSH Item No_] = f_csh.[CSH Item No_])


select 
	*
	into PAK_CALCULATION_GOLD_TEMP_2
from PAK_CALCULATION_GOLD_TEMP_3 t
where [CSH Document Date Time] = (select max([CSH Document Date Time]) from [GOLD_Warehouse].[dbo].[f_csh_cost_share] doc_time_csh
					where [CSH Indentation] = 0 and [CSH Is Leaf] = 0
						 and doc_time_csh.[CSH Item No_] = t.[CSH Item No_]
						 and t.[CSH On Date] = doc_time_csh.[CSH On Date])

select
	[PRD Prod_ Order No_],
	prd.[ITM Key],
	t.[CSH Document No_]
	into PAK_CALCULATION_GOLD_TEMP_1
from [GOLD_Warehouse].[dbo].[f_prd_production_order_line] prd
	inner join PAK_CALCULATION_GOLD_TEMP_2 t on t.[CSH Company] + t.[CSH Item No_] = prd.[ITM Key]
where prd.[PRD Line No_] = 10000

select
	ena.*,
	csh.[CSH Unit Cost] AS [CSH Last Unit Cost],
	csh.[CSH Total Cost] AS [CSH Last Total Cost]
INTO PAK_CALCULATION_GOLD_TEMP
from PAK_CALCULATION_GOLD_TEMP_1 ena
	inner join [GOLD_Warehouse].[dbo].[f_csh_cost_share] csh on ena.[CSH Document No_] = csh.[CSH Document No_]
where [CSH Indentation] = 0 and [CSH Is Leaf] = 0

------------------------------------------------------

SELECT 
	[PRS wo_code],
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, 1, 0) [Has Downtime],
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([1-MATERIAL] AS FLOAT) / ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [1-MATERIAL],
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([2-MASA] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [2-MASA],
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([3-PRELIV] AS FLOAT) / ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [3-PRELIV],
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([4-STROJNA OKVARA] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [4-STROJNA OKVARA] ,
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([5-KADER] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [5-KADER] ,
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([6-PLAN] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [6-PLAN],
	IIF(([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) = 0, NULL, CAST([1-MATERIAL] AS FLOAT) / ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) 
	+ CAST([2-MASA] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN]) 
	+ CAST([3-PRELIV] AS FLOAT) / ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])
	+ CAST([4-STROJNA OKVARA] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])
	+ CAST([5-KADER] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])
	+ CAST([6-PLAN] AS FLOAT)/ ([1-MATERIAL] + [2-MASA] +[3-PRELIV]+ [4-STROJNA OKVARA]+ [5-KADER]+[6-PLAN])) [Number of downtimes],
	Comment,
	[Downtime Duration PAK],
	[Downtime Duration Total]
	INTO PAK_DOWNTIME_GOLD_TEMP
	FROM (
		SELECT 
			[PRS wo_code],
			CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(200)) AS Comment,
			SUM(CASE WHEN [PRS code_erp] LIKE '%PAK%' THEN [PRS DT] ELSE 0 END) AS [Downtime Duration PAK],
			SUM([PRS DT]) AS [Downtime Duration Total],
			CASE WHEN UPPER(CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(100))) LIKE '%DE1-MATERIAL%' THEN 1 ELSE 0 END AS [1-MATERIAL],
			CASE WHEN UPPER(CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(100))) LIKE '%DE2-MASA%' THEN 1 ELSE 0 END AS [2-MASA],
			CASE WHEN UPPER(CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(100))) LIKE '%DE3-PRELIV%' THEN 1 ELSE 0 END AS [3-PRELIV],
			CASE WHEN UPPER(CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(100))) LIKE '%DE4-STROJNA OKVARA%' THEN 1 ELSE 0 END AS [4-STROJNA OKVARA],
			CASE WHEN UPPER(CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(100))) LIKE '%DE5-KADER%' THEN 1 ELSE 0 END AS [5-KADER],
			CASE WHEN UPPER(CAST(STRING_AGG(left([PRS comment],3500), ' ') AS VARCHAR(100))) LIKE '%DE6-PLAN%' THEN 1 ELSE 0 END AS [6-PLAN]
		FROM 
			[GOLD_Warehouse].[dbo].[f_prs_rs_woo_production_report_sel]
		GROUP BY 
			[PRS wo_code]
	) PRS


------------------------------------------------------

SELECT 
    [PRD Prod_ Order No_],
    IIF([PRD Quantity] = 0, 0,[PRD Finished Quantity] / [PRD Quantity]) AS [Order Realisation]
INTO  PAK_REALISATION_GOLD_TEMP
FROM 
    f_prd_production_order_line
WHERE 
    [PRD Line No_] = 10000
  

------------------------------------------------------

insert into f_pak_postcalculation_analysis

SELECT * /*INTO PAK_FINAL_GOLD_TEMP*/ 
--into  [dbo].[f_pak_postcalculation_analysis] 
FROM(
 SELECT 
	'Item' [PAK Item/SubItem Description],
	1 [PAK Item/SubItem],
	prd.[PRD Prod_ Order No_],
	prd.[NAV Company] [PAK Company],
    prd1.[PRD Routing Group] AS [PAK Routing Group],
    prd.[PRD Starting Date] AS [PAK Date],
    prd.[PRD Status] AS [PAK Status], 
    CASE 
		WHEN UPPER([ITM Description]) LIKE '%MIX TK%' THEN 'PAK'
        WHEN [ITM Inventory Posting Group] = 'IZDELKI' THEN 'I' 
        WHEN [ITM Inventory Posting Group] = 'POLIZDELEK' THEN 'P' 
        ELSE [ITM Inventory Posting Group] 
    END AS [PAK I/P],
    itm.[ITM No_]  AS [PAK No_],
	CASE WHEN [ITM Item No - Description] IS NULL THEN itm.[ITM No_] ELSE  [ITM Item No - Description] END  [PAK Item No_ Description],
	prd.[NAV Company] + itm.[ITM No_] [ITM Key],
	prd.[NAV Company] +  prd.[PRD Prod_ Order No_]  AS [PRD Key],
   [ITM Description] AS [PAK Description],
    [ITM Customer Group] AS [PAK Customer Group],
    [ITM Customer Group Code] AS [PAK Customer Group Code],
    prd.[PRD Quantity (Base)] AS [PAK Quantity],
    prd.[PRD Finished Qty_ (Base)] AS [PAK Finished Quantity], 
    CAST([TK/MP Convertor]  AS NUMERIC(18,4)) AS [PAK Convertor],
    CAST([TK/MP Convertor] * prd.[PRD Quantity (Base)]  AS NUMERIC(18,4)) AS [PAK Quantity PE],
    [ITM Base Unit of Measure] AS [PAK Base Unit of Measure],
    CAST([TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)] AS NUMERIC(18,4)) AS [PAK Finished Quantity PE],
   [ITM Sales Unit of Measure] AS [PAK Sales Unit of Measure],
	CAST(
	CASE 
		WHEN [ITM Base Unit of Measure] = 'KARTON' THEN prd.[PRD Finished Qty_ (Base)]
		WHEN [ITM Sales Unit of Measure] = 'KARTON' THEN [TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)]
		ELSE (1 / ium.[IUM Qty_ per Unit of Measure]) * ([TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)])
	END 
	AS NUMERIC(18,4)) AS [PAK Finished Quantity per KARTON], -- dodano za potrebe strani Povprecni DIS na Analitiki proizvodnje (vse zelijo gledat v kartonih, iz tega izracunana povprecni DIS in PP LC 1) - Urska, 9.1.2025
     IIF(prd.[PRD Quantity (Base)] * [PRD Finished Qty_ (Base)] = 0 , NULL, 100 / prd.[PRD Quantity (Base)] * [PRD Finished Qty_ (Base)]) AS [PAK Quantity Efficiency],
	SUM([Posted Production Value]) AS [PAK Posted Production Value],
    SUM([Planned Production Value]) AS [PAK Planned Production Value],
	CAST(IIF(([TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)]) = 0, NULL,SUM([Posted Production Value])  / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)]))			AS NUMERIC(18,4)) AS [PAK Posted DIS],
	CAST(IIF(([TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)]) = 0, NULL, SUM([Planned Production Value])  / ([TK/MP Convertor] *   prd.[PRD Finished Qty_ (Base)]))	AS NUMERIC(18,4)) AS [PAK Planned DIS],
	CAST(IIF(([TK/MP Convertor] * prd.[PRD Quantity (Base)]) = 0, NULL, SUM([Posted Production Value])  / ([TK/MP Convertor] *   prd.[PRD Quantity (Base)]))				AS NUMERIC(18,4)) AS [PAK Order DIS],
    SUM([Posted Production Value]) - ISNULL(SUM([Planned Production Value]),0) AS [PAK Difference Production Amount],
	IIF(SUM([Planned Production Value]) = 0, NULL, (SUM([Posted Production Value]) / SUM([Planned Production Value]) )-1) AS [PAK Deviation Amount Production], -- procent
	SUM([Posted Hours Amount]) AS [PAK Posted Hours Amount] ,
    SUM([Posted Hours Quantity]) AS [PAK Posted Hours Quantity],
    SUM([Planned Hours Amount]) AS [PAK Planned Hours Amount],
    SUM([Planned Hours Quantity]) AS [PAK Planned Hours Quantity],
    -(SUM([Planned Hours Amount]) - ISNULL(SUM([Posted Hours Amount]),0)) AS [PAK Deviation Hours Amount],
    IIF(SUM([Planned Hours Quantity]) = 0, NULL, (SUM([Posted Hours Quantity]) / SUM([Planned Hours Quantity]))-1) AS [PAK Hours Deviation %], -- procent
    SUM([Posted Work Center Amount]) AS [PAK Posted Work Center Amount],
    SUM([Planned Work Center Amount]) AS [PAK Planned Work Center Amount],
    SUM([Posted Material Cost Amount]) AS [PAK Posted Material Cost Amount],
    SUM([Planned Material Cost Amount]) AS [PAK Planned Material Cost Amount],
     -(SUM([Planned Material Cost Amount]) - ISNULL(SUM([Posted Material Cost Amount]),0)) AS [PAK Material Cost Deviation Amount],
    IIF(SUM([Planned Material Cost Amount])  = 0, NULL, SUM([Posted Material Cost Amount]) / SUM([Planned Material Cost Amount]) ) -1 AS [Material Deviation %], -- procent
	IIF(SUM([Posted Production Value]) = 0, NULL, SUM([Posted Material Cost Amount]) / SUM([Posted Production Value])) [PAK % Material],
    IIF(SUM([Posted Production Value]) = 0, NULL, SUM([Posted Hours Amount]) / SUM([Posted Production Value]))AS [PAK % Work],
    IIF(SUM([Posted Production Value]) = 0, NULL, SUM([Posted Work Center Amount]) / SUM([Posted Production Value]) )AS [PAK % Energy],
    IIF(SUM([Posted Production Value]) = 0, NULL, SUM([Posted Material Cost Amount]) / SUM([Posted Production Value])) + IIF(SUM([Posted Production Value]) = 0, NULL, SUM([Posted Hours Amount]) / SUM([Posted Production Value])) + IIF(SUM([Posted Production Value]) = 0, NULL, SUM([Posted Work Center Amount]) / SUM([Posted Production Value])) AS [PAK Check %],  
	convert(float,IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, (SUM([Posted Production Value]) - ISNULL(SUM([Posted Hours Amount]),0) - ISNULL(SUM([Posted Work Center Amount]),0)) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)]))) AS [PAK Cost Material],
    CAST(IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, (SUM([Posted Production Value]) - ISNULL(SUM([Posted Work Center Amount]),0) - ISNULL(SUM([Posted Material Cost Amount]),0)) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])) AS NUMERIC(18,4)) 	AS [PAK Cost Work],
    CAST(IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, (SUM([Posted Production Value]) - ISNULL(SUM([Posted Hours Amount]),0) - ISNULL(SUM([Posted Material Cost Amount]),0)) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)]))		AS NUMERIC(18,4)) 	AS [PAK Cost Energy],
    CAST(
		IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, SUM([Posted Production Value])  / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])) 
		- ((SUM([Posted Production Value]) - ISNULL(SUM([Posted Hours Amount]),0) - ISNULL(SUM([Posted Work Center Amount]),0))  + (SUM([Posted Production Value]) - ISNULL(SUM([Posted Work Center Amount]),0) - ISNULL(SUM([Posted Material Cost Amount]),0)) + (ISNULL(SUM([Posted Production Value]),0) - ISNULL(SUM([Posted Hours Amount]),0) - ISNULL(SUM([Posted Material Cost Amount]),0)) ) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])) 
	AS NUMERIC(18,4)) AS [PAK Check Cost],
	CASE 
		WHEN UPPER([ITM Description]) LIKE '%MIX TK%' THEN SUM([Posted Material Cost Amount]) ELSE  NULL 
	END [PAK POM Semifinished Item Consumption] ,
	SUM([Manufacturing Posted Quantity]) [PAK Posted Routing Operating Hours],
	SUM([Manufacturing Planned Quantity]) [PAK Planned Routing Operating Hours],
	NULL [PAK Routing Total Hours of Unfinished Production],
	NULL [PAK Routing Packaking Hours],
	
	-- kalkulacije -- 
	-- obdobje: kvartal zacetka DNI-ja
	-- imen stolpcev v končni tabeli ne menjam, da ni treba menjat v modelih (niso cist opisni ampak niso pa slabi)
	csh4.[CSH Unit Cost in Quarter] [PAK Calculation on Date], -- zadnja kalkulacija za ta artikel v obdobju
	calc.[CSH Last Unit Cost] [PAK Last Calculation on Date], -- zadnja kalkulacija za ta artikel in general
	IIF(
		([TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)]) = 0 OR [CSH Unit Cost in Quarter] = 0, 
		NULL,
		(SUM([Posted Production Value])  / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])) / [CSH Unit Cost in Quarter] 
	) AS [PAK Calculation Deviation],
	csh4.[CSH Total Cost in Quarter] AS [PAK Base Total Calculation Cost], 
	calc.[CSH Last Total Cost] [PAK Last Total Calculation Cost], 
	----------------
	
	IIF([PRC Price per KARTON] is NULL, 0, [PRC Price per KARTON]) [PAK Average Item Price],
	CAST(IIF([PRC Price per KARTON] = 0, NULL,  [TK/MP Convertor] *[PRD Finished Qty_ (Base)] *[PRC Price per KARTON]) AS NUMERIC (18,4)) [PAK Sales Amount],
	CAST(
		IIF(
			SUM([Posted Production Value]) = 0, 
			NULL, 
			([TK/MP Convertor] *[PRD Finished Qty_ (Base)] *[PRC Price per KARTON]) / SUM([Posted Production Value])) 
	AS NUMERIC(18,4)) AS [PAK Gross Value], --bruto masa kritja
	CAST(([TK/MP Convertor] *prd.[PRD Finished Qty_ (Base)] *[PRC Price per KARTON]) - ISNULL(SUM([Posted Production Value]),0) AS NUMERIC(18,4)) AS [PAK Gross Mass Value],
	CAST(-([TK/MP Convertor] * prd.[PRD Finished Qty_ (Base)] - [TK/MP Convertor] * prd.[PRD Quantity (Base)] ) * [PRC Price per KARTON] AS NUMERIC(18,4)) [PAK Sales Difference Quantity Value],
    PAK_DOWNTIME_GOLD_TEMP.Comment [PAK Comment],
	[Has Downtime] [PAK Has Downtime],
	[1-MATERIAL] [PAK 1-MATERIAL],
	[2-MASA] [PAK 2-MASA],
	[3-PRELIV] [PAK 3-PRELIV],
	[4-STROJNA OKVARA] [PAK 4-STROJNA OKVARA],
	[5-KADER] [PAK 5-KADER],
	[6-PLAN] [PAK 6-PLAN],
	[Number of downtimes] [PAK Number of downtimes],
	(SUM([Posted Work Center Amount]) + SUM([Posted Material Cost Amount])+ SUM([Posted Hours Amount])) -  SUM([Posted Production Value]) [PAK Check Production Value],
	[Downtime Duration PAK] [PAK Downtime Time PAK],
	[Downtime Duration Total] [PAK Downtime Duration Total],
	-(SUM([Posted Work Center Amount]) - SUM([Planned Work Center Amount])) [PAK Material Work Center Deviation Amount],
	(((-(SUM([Planned Material Cost Amount]) - SUM([Posted Material Cost Amount]))) + (-(SUM([Planned Hours Amount]) - SUM([Posted Hours Amount]))))- (SUM([Posted Production Value]) - SUM([Planned Production Value]))) [PAK Work Center Deviation],
	(-(SUM([Posted Work Center Amount]) - SUM([Planned Work Center Amount])))-((((-(SUM([Planned Material Cost Amount]) - SUM([Posted Material Cost Amount]))) + (-(SUM([Planned Hours Amount]) -SUM([Posted Hours Amount]))))- (SUM([Posted Production Value]) - SUM([Planned Production Value])))) [PAK Work Center Deviation Difference]
	, SUM([Order Ammount]) [PAK Order Ammount]
	,NULL AS [PAK Task Type]
	,NULL AS [PAK Task Type Desription]
	,NULL AS [PAK Type]
	,NULL AS [PAK Type Description]
	,SUM([PCF Order Qty_]) [PAK Order Qty_]--
	,SUM([PCF Planned Unit cost]) [PAK Planned Unit cost]
	,SUM([PCF Order Unit Cost]) [PAK Order Unit Cost]
	,SUM([PCF Posted Unit Cost]) [PAK Posted Unit cost]
	,MAX([PCF Description 2]) [PAK Description 2]
	,SUM([PCF Posted Unit Cost (Expected)]) [PAK Posted Unit Cost (Expected)]
	,SUM([PCF Posted Cost Ammount (Expected)]) [PAK Posted Cost Ammount (Expected)]
	,sum([PCF Order Qty_] *[Order Realisation]) AS [PAK Order Quantity Realisation]
	,sum([PCF Order Ammount] * [Order Realisation]) AS [PAK Order Value Realisation]
FROM 
   f_prd_production_order_line prd 
LEFT JOIN
	f_prd_production  prd1 on prd.[PRD Prod_ Order No_] = prd1.[PRD No_]

LEFT JOIN 
    f_pcf_post_calculation_buffer buf ON buf.[PCF Prod_ Order No_] = prd.[PRD Prod_ Order No_]
LEFT JOIN	
	PAK_CONVERTOR_GOLD_TEMP CONV ON CONV.[ITM Key] = prd.[ITM Key]
LEFT JOIN 
	PAK_CSH_CONNECTION_TEMP_1 csh4 ON csh4.[PRD Prod_ Order No_] = prd.[PRD Prod_ Order No_] 
LEFT JOIN 
	PAK_CALCULATION_GOLD_TEMP calc ON calc.[PRD Prod_ Order No_] = prd.[PRD Prod_ Order No_] 
LEFT JOIN 
	PAK_REALISATION_GOLD_TEMP ON PAK_REALISATION_GOLD_TEMP.[PRD Prod_ Order No_] = prd.[PRD Prod_ Order No_]
LEFT JOIN
	PAK_PRICE_GOLD_TEMP prc ON prd.[ITM Key] = prc.[ITM Key] and prd.[PRD Prod_ Order No_] = prc.[PRD Prod_ Order No_]
LEFT JOIN	
	PAK_DOWNTIME_GOLD_TEMP ON PAK_DOWNTIME_GOLD_TEMP.[PRS wo_code] = prd.[PRD Prod_ Order No_] 
LEFT JOIN 
      d_itm_item itm ON itm.[ITM Key] = prd.[ITM Key]
LEFT JOIN 
    PAK_PRODUCTION_GOLD_TEMP PR ON PR.[Prod_ Order No_] = prd.[PRD Prod_ Order No_] 
					AND PR.[No_] = buf.[PCF No_]
					AND PR.[No_] = buf.[PCF No_]
					AND PR.[Task Type] = buf.[PCF Task Type]
					AND PR.[Type] = buf.[PCF Type]
LEFT JOIN 
    PAK_HOURSS_GOLD_TEMP HR ON HR.[Prod_ Order No_] =  prd.[PRD Prod_ Order No_]
					AND HR.[No_] = buf.[PCF No_]
					AND HR.[Task Type] = buf.[PCF Task Type]
					AND HR.[Type] = buf.[PCF Type]

LEFT JOIN 
    PAK_WORK_CENTER_GOLD_TEMP WC ON WC.[Prod_ Order No_] = prd.[PRD Prod_ Order No_]
					AND WC.[No_] = buf.[PCF No_]
					AND WC.[Task Type] = buf.[PCF Task Type]
					AND WC.[Type] = buf.[PCF Type]
LEFT JOIN 
    PAK_MATERIAL_GOLD_TEMP MT ON MT.[Prod_ Order No_] =  prd.[PRD Prod_ Order No_]
					AND MT.[No_] = buf.[PCF No_]
					AND MT.[Task Type] = buf.[PCF Task Type]
					AND MT.[Type] = buf.[PCF Type]

LEFT JOIN 
	PAK_ROUTING_HOURS_GOLD_TEMP RHO ON RHO.[Prod_ Order No_] = prd.[PRD Prod_ Order No_]
					AND RHO.[No_] = buf.[PCF No_]
					AND RHO.[Task Type] = buf.[PCF Task Type]
					AND RHO.[Type] = buf.[PCF Type]
LEFT JOIN 
	f_ium_item_unit_of_measure ium ON prd.[ITM Key] = ium.[ITM Key] and ium.[IUM Code] = itm.[ITM Sales Unit of Measure]
where prd.[PRD Line No_] =10000  --and buf.[PCF No_] not in ('PAK', 'POL') and prd.[PRD Prod_ Order No_] =  'DNI24-04078'


GROUP BY 
	 prd.[NAV Company],
    prd.[PRD Status] ,
    prd1.[PRD Routing Group], 
    prd.[PRD Starting Date],
    CASE 
        WHEN [ITM Inventory Posting Group] = 'IZDELKI' THEN 'I' 
        WHEN [ITM Inventory Posting Group] = 'POLIZDELEK' THEN 'P'  
        ELSE [ITM Inventory Posting Group] 
    END ,
    itm.[ITM No_],
	CASE WHEN [ITM Item No - Description] IS NULL THEN itm.[ITM No_] ELSE  [ITM Item No - Description] END  ,
    prd.[PRD Prod_ Order No_] ,
    [ITM Description],
    [ITM Customer Group],
    [ITM Customer Group Code] ,
    prd.[PRD Quantity (Base)] ,
    prd.[PRD Finished Qty_ (Base)] ,
    [TK/MP Convertor],
    [ITM Base Unit of Measure],
    [CSH Unit Cost in Quarter] ,
    [CSH Last Unit Cost],
    [ITM Sales Unit of Measure],
	Comment,
	[Has Downtime],
	[PRC Price per KARTON],
	[1-MATERIAL],
	[2-MASA],
	[3-PRELIV],
	[4-STROJNA OKVARA],
	[5-KADER],
	[6-PLAN],
	[Number of downtimes],
	[Downtime Duration PAK],
	[Downtime Duration Total],
	[ITM Inventory Posting Group]
	,[CSH Total Cost in Quarter] 
	,[CSH Last Total Cost] 
	,[IUM Qty_ per Unit of Measure]
	
UNION ALL

 SELECT 
	'Sub Item' [PAK Item/SubItem Description],
	0 [PAK Item/SubItem],
	prd.[PRD Prod_ Order No_],
	prd.[NAV Company] [PAK Company],
    prd1.[PRD Routing Group] AS [PAK Routing Group],
    prd.[PRD Starting Date] AS [PAK Date],
    prd.[PRD Status] AS [PAK Status], 
    CASE 
		WHEN UPPER([ITM Description]) LIKE '%MIX TK%' THEN 'PAK'
        WHEN [ITM Inventory Posting Group] = 'IZDELKI' THEN 'I' 
        WHEN [ITM Inventory Posting Group] = 'POLIZDELEK' THEN 'P' 
        ELSE [ITM Inventory Posting Group] 
    END AS [PAK I/P],
    [PCF No_]  AS [PAK No_],
	CASE WHEN [ITM Item No - Description] IS NULL THEN [PCF No_] ELSE  [ITM Item No - Description] END   [PAK Item No_ Description] ,
	prd.[NAV Company] + [PCF No_]  [ITM Key],
	prd.[NAV Company] +  prd.[PRD Prod_ Order No_]  AS [PRD Key],
    [ITM Description]  AS [PAK Description],
    [ITM Customer Group] AS [PAK Customer Group],
    [ITM Customer Group Code] AS [PAK Customer Group Code],
    [PCF Planned Qty_]  AS [PAK Quantity],
    [PCF Posted Qty_] AS [PAK Finished Quantity], 
    CAST([TK/MP Convertor] AS NUMERIC(18,4)) AS [PAK Convertor],
    CAST([TK/MP Convertor] * [PCF Planned Qty_] AS NUMERIC(18,4)) AS [PAK Quantity PE],
    [ITM Base Unit of Measure] AS [PAK Base Unit of Measure],
    CAST([TK/MP Convertor] * [PCF Posted Qty_] AS NUMERIC(18,4)) AS [PAK Finished Quantity PE],
    [ITM Sales Unit of Measure] AS [PAK Sales Unit of Measure],
	CAST(
	CASE 
		WHEN [ITM Base Unit of Measure] = 'KARTON' THEN [PCF Posted Qty_]
		WHEN [ITM Sales Unit of Measure] = 'KARTON' THEN [TK/MP Convertor] * [PCF Posted Qty_]
		ELSE (1 / ium.[IUM Qty_ per Unit of Measure]) * ([TK/MP Convertor] * [PCF Posted Qty_])
	END 
	AS NUMERIC(18,4)) AS [PAK Finished Quantity per KARTON], -- dodano za potrebe strani Povprecni DIS na Analitiki proizvodnje (vse zelijo gledat v kartonih, iz tega izracunana povprecni DIS in PP LC 1) - Urska, 9.1.2025
    IIF([PCF Planned Qty_] * [PCF Posted Qty_] = 0 , NULL, 100 / [PCF Planned Qty_] * [PCF Posted Qty_]) AS [PAK Quantity Efficiency],
    [Posted Production Value] AS [PAK Posted Production Value],
    [Planned Production Value] AS [PAK Planned Production Value],
    CAST(IIF(([TK/MP Convertor] * [PCF Posted Qty_]) = 0, NULL, [Posted Production Value]  / ([TK/MP Convertor] * [PCF Posted Qty_]))	AS NUMERIC(18,4)) AS [PAK Posted DIS],
    CAST(IIF(([TK/MP Convertor] * [PCF Planned Qty_]) = 0, NULL, [Planned Production Value]  / ([TK/MP Convertor] * [PCF Planned Qty_]))AS NUMERIC(18,4)) AS [PAK Planned DIS],
    CAST(IIF(([TK/MP Convertor] * [PCF Posted Qty_]) = 0, NULL, [Posted Production Value]  / ([TK/MP Convertor] * [PCF Posted Qty_]))	AS NUMERIC(18,4)) AS [PAK Order DIS],
    [Posted Production Value] - [Planned Production Value] AS [PAK Difference Production Amount],
    IIF([Planned Production Value] = 0, NULL,( [Posted Production Value] / [Planned Production Value] )-1) AS [PAK Deviation Amount Production], --
    [Posted Hours Amount] AS [PAK Posted Hours Amount] ,
    [Posted Hours Quantity] AS [PAK Posted Hours Quantity],
    [Planned Hours Amount] AS [PAK Planned Hours Amount],
    [Planned Hours Quantity] AS [PAK Planned Hours Quantity],
    -([Planned Hours Amount] - [Posted Hours Amount]) AS [PAK Deviation Hours Amount],
    IIF([Planned Hours Quantity] = 0, NULL, ([Posted Hours Quantity] / [Planned Hours Quantity])-1) AS [PAK Hours Deviation %], --
    [Posted Work Center Amount] AS [PAK Posted Work Center Amount],
    [Planned Work Center Amount] AS [PAK Planned Work Center Amount],
    [Posted Material Cost Amount] AS [PAK Posted Material Cost Amount],
    [Planned Material Cost Amount] AS [PAK Planned Material Cost Amount],
    -([Planned Material Cost Amount] - [Posted Material Cost Amount]) AS [PAK Material Cost Deviation Amount],
    IIF([Planned Material Cost Amount]  = 0, NULL, [Posted Material Cost Amount] / [Planned Material Cost Amount] )-1 AS [Material Deviation %], --
	IIF([Posted Production Value] = 0, NULL, [Posted Material Cost Amount] / [Posted Production Value]) [PAK % Material],
    IIF([Posted Production Value] = 0, NULL, [Posted Hours Amount] / [Posted Production Value]) AS [PAK % Work],
    IIF([Posted Production Value] = 0, NULL, [Posted Work Center Amount] / [Posted Production Value]) AS [PAK % Energy],
    IIF([Posted Production Value] = 0, NULL, [Posted Material Cost Amount] / [Posted Production Value]) + IIF([Posted Production Value] = 0, NULL, [Posted Hours Amount] / [Posted Production Value]) + IIF([Posted Production Value] = 0, NULL, [Posted Work Center Amount] / [Posted Production Value]) AS [PAK Check %],
	CAST(IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, ([Posted Production Value] - ISNULL([Posted Hours Amount],0) - ISNULL([Posted Work Center Amount],0)) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)]))			AS NUMERIC(18,4)) AS [PAK Cost Material],
    CAST(IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, ([Posted Production Value] - ISNULL([Posted Work Center Amount],0) - ISNULL([Posted Material Cost Amount],0))) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])	AS NUMERIC(18,4)) AS [PAK Cost Work],
    CAST(IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, ([Posted Production Value] - ISNULL([Posted Hours Amount],0) - ISNULL([Posted Material Cost Amount],0)) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)]))		AS NUMERIC(18,4)) AS [PAK Cost Energy],
    CAST(
		IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, IIF(([TK/MP Convertor] * [PRD Finished Qty_ (Base)]) = 0, NULL, [Posted Production Value]  / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])) 
		- (([Posted Production Value] - ISNULL([Posted Hours Amount],0) - ISNULL([Posted Work Center Amount],0))  + ([Posted Production Value] - ISNULL([Posted Work Center Amount],0) - ISNULL([Posted Material Cost Amount],0) + ([Posted Production Value] - ISNULL([Posted Hours Amount],0) - ISNULL([Posted Material Cost Amount],0) ) / ([TK/MP Convertor] * [PRD Finished Qty_ (Base)])))) 
	AS NUMERIC(18,4)) AS [PAK Check Cost],
	CASE 
		WHEN UPPER([ITM Description]) LIKE '%MIX TK%' THEN [Posted Material Cost Amount] ELSE  NULL 
	END [PAK POM Semifinished Item Consumption] ,
	[Manufacturing Posted Quantity] [PAK Posted Routing Operating Hours],
	[Manufacturing Planned Quantity] [PAK Planned Routing Operating Hours],
	NULL [PAK Routing Total Hours of Unfinished Production],
	NULL [PAK Routing Packaking Hours],

	null as [PAK Calculation on Date], 
	null as [PAK Last Calculation on Date],
	null as [PAK Calculation Deviation],
	null AS [PAK Base Total Calculation Cost], 
	null as [PAK Last Total Calculation Cost], 
	/*
	-- kalkulacije --Žiga potrdil, da so lahko ta polja samo pri 'item' in ne pri 'subitem'
	-- obdobje: kvartal zacetka DNI-ja
	-- imen stolpcev v končni tabeli ne menjam, da ni treba menjat v modelih (niso cist opisni ampak niso pa slabi)
	csh4.[CSH Unit Cost in Quarter] [PAK Calculation on Date], -- zadnja kalkulacija za ta artikel v obdobju
	calc.[CSH Last Unit Cost] [PAK Last Calculation on Date], -- zadnja kalkulacija za ta artikel in general
	NULL [PAK Calculation Deviation],
	csh4.[CSH Total Cost in Quarter] AS [PAK Base Total Calculation Cost], 
	calc.[CSH Last Total Cost] [PAK Last Total Calculation Cost], 
	----------------
	*/
	IIF([PRC Price per KARTON] IS NULL, 0, [PRC Price per KARTON]) [PAK Average Item Price],
	CAST(IIF([PRC Price per KARTON] = 0, NULL,  [TK/MP Convertor] *[PCF Posted Qty_] *[PRC Price per KARTON]) AS NUMERIC(18,4)) [PAK Sales Amount],
	CAST(IIF([Posted Production Value] = 0, NULL, ([TK/MP Convertor] *[PCF Posted Qty_] *[PRC Price per KARTON]) / [Posted Production Value]) AS NUMERIC(18,4)) [PAK Gross Value], --bruto masa kritja
	CAST(([TK/MP Convertor] *[PCF Posted Qty_] *[PRC Price per KARTON]) - [Posted Production Value] AS NUMERIC(18,4)) [PAK Gross Mass Value],
	CAST(-([TK/MP Convertor] * [PCF Posted Qty_] - [TK/MP Convertor] * [PCF Planned Qty_] ) * [PRC Price per KARTON] AS NUMERIC(18,4)) [PAK Sales Difference Quantity Value],
	Comment [PAK Comment],
	[Has Downtime] [PAK Has Downtime],
	[1-MATERIAL] [PAK 1-MATERIAL],
	[2-MASA] [PAK 2-MASA],
	[3-PRELIV] [PAK 3-PRELIV],
	[4-STROJNA OKVARA] [PAK 4-STROJNA OKVARA],
	[5-KADER] [PAK 5-KADER],
	[6-PLAN] [PAK 6-PLAN],
	[Number of downtimes] [PAK Number of downtimes],
	([Posted Work Center Amount] + [Posted Material Cost Amount] + [Posted Hours Amount]) -  [Posted Production Value] [PAK Check Production Value],
	[Downtime Duration PAK] [PAK Downtime Time PAK],
	[Downtime Duration Total] [PAK Downtime Duration Total],
	-([Posted Work Center Amount] - ISNULL([Planned Work Center Amount],0)) [PAK Material Work Center Deviation Amount],
	(((-([Planned Material Cost Amount] - ISNULL([Posted Material Cost Amount],0))) + (-([Planned Hours Amount] - ISNULL([Posted Hours Amount],0))))- ([Posted Production Value] - ISNULL([Planned Production Value],0))) [PAK Work Center Deviation],
	(-([Posted Work Center Amount] - ISNULL([Planned Work Center Amount],0)))-((((-([Planned Material Cost Amount] - ISNULL([Posted Material Cost Amount],0))) + (-([Planned Hours Amount] - ISNULL([Posted Hours Amount],0))))- ([Posted Production Value] - ISNULL([Planned Production Value],0)))) [PAK Work Center Deviation Difference]
	,[PCF Order Ammount] [PAK Order Ammount]
	,[PCF Task Type] AS [PAK Task Type]
	,[PCT Task Type Description] AS [PAK Task Type Desription]
	,[PCF Type] AS [PAK Type]
	,[PCF Type Description] AS [PAK Type Description]
	,[PCF Order Qty_] [PAK Order Qty_]
	,[PCF Planned Unit cost] [PAK Planned Unit cost]
	,[PCF Order Unit Cost] [PAK Order Unit Cost]
	,[PCF Posted Unit Cost] [PAK Posted Unit cost]
	,[PCF Description 2] [PAK Description 2]
	,[PCF Posted Unit Cost (Expected)] [PAK Posted Unit Cost (Expected)]
	,[PCF Posted Cost Ammount (Expected)] [PAK Posted Cost Ammount (Expected)]
	,[PCF Order Qty_] *[Order Realisation] AS [PAK Order Quantity Realisation]
	,[PCF Order Ammount] * [Order Realisation] AS [PAK Order Value Realisation]
FROM 
   f_prd_production_order_line prd
LEFT JOIN
	f_prd_production prd1 on prd.[PRD Prod_ Order No_] = prd1.[PRD No_]

LEFT JOIN 
    f_pcf_post_calculation_buffer buf ON buf.[PCF Prod_ Order No_] = prd.[PRD Prod_ Order No_]
LEFT JOIN	
	PAK_CONVERTOR_GOLD_TEMP CONV ON CONV.[ITM Key] = buf.[ITM Key]
--LEFT JOIN PAK_CSH_CONNECTION_TEMP_1 csh4 ON csh4.[PRD Prod_ Order No_] = prd.[PRD Prod_ Order No_] 
--LEFT JOIN PAK_CALCULATION_GOLD_TEMP calc ON calc.[PRD Prod_ Order No_] = prd.[PRD Prod_ Order No_] 
LEFT JOIN 
	PAK_REALISATION_GOLD_TEMP ON PAK_REALISATION_GOLD_TEMP.[PRD Prod_ Order No_] = prd.[PRD Prod_ Order No_]
LEFT JOIN 
	PAK_PRICE_GOLD_TEMP prc ON prd.[ITM Key] = prc.[ITM Key] and prd.[PRD Prod_ Order No_] = prc.[PRD Prod_ Order No_]
LEFT JOIN	
	PAK_DOWNTIME_GOLD_TEMP ON PAK_DOWNTIME_GOLD_TEMP.[PRS wo_code] = prd.[PRD Prod_ Order No_] 
LEFT JOIN 
      d_itm_item itm ON itm.[ITM No_] = buf.[PCF No_] 
LEFT JOIN 
    PAK_PRODUCTION_GOLD_TEMP PR ON PR.[Prod_ Order No_] = prd.[PRD Prod_ Order No_]  
					AND PR.[No_] = buf.[PCF No_]
					AND PR.[No_] = buf.[PCF No_]
					AND PR.[Task Type] = buf.[PCF Task Type]
					AND PR.[Type] = buf.[PCF Type]
LEFT JOIN 
    PAK_HOURSS_GOLD_TEMP HR ON HR.[Prod_ Order No_] =  prd.[PRD Prod_ Order No_]
					AND HR.[No_] = buf.[PCF No_]
					AND HR.[Task Type] = buf.[PCF Task Type]
					AND HR.[Type] = buf.[PCF Type]
LEFT JOIN 
    PAK_WORK_CENTER_GOLD_TEMP WC ON WC.[Prod_ Order No_] = prd.[PRD Prod_ Order No_]
					AND WC.[No_] = buf.[PCF No_]
					AND WC.[Task Type] = buf.[PCF Task Type]
					AND WC.[Type] = buf.[PCF Type]
LEFT JOIN 
    PAK_MATERIAL_GOLD_TEMP MT ON MT.[Prod_ Order No_] =  prd.[PRD Prod_ Order No_]
					AND MT.[No_] = buf.[PCF No_]
					AND MT.[Task Type] = buf.[PCF Task Type]
					AND MT.[Type] = buf.[PCF Type]

LEFT JOIN PAK_ROUTING_HOURS_GOLD_TEMP RHO ON RHO.[Prod_ Order No_] = prd.[PRD Prod_ Order No_]
					AND RHO.[No_] = buf.[PCF No_]
					AND RHO.[Task Type] = buf.[PCF Task Type]
					AND RHO.[Type] = buf.[PCF Type]
LEFT JOIN f_ium_item_unit_of_measure ium on itm.[ITM Key] = ium.[ITM Key]and ium.[IUM Code] = itm.[ITM Sales Unit of Measure]

WHERE 
    prd.[PRD Line No_] = 10000 --  and prd.[PRD Prod_ Order No_] =  'DNI24-04078'
	--and [PRD Prod_ Order No_] ='DNI23-31851' 
	) A



------------------------------------------------------
------------------------------------------------------
--SELECT * FROM  PAK_FINAL_GOLD_TEMP
--------------------------------

DROP TABLE IF EXISTS PAK_PRODUCTION_GOLD_TEMP
DROP TABLE IF EXISTS PAK_HOURSS_GOLD_TEMP
DROP TABLE IF EXISTS PAK_ROUTING_HOURS_GOLD_TEMP
DROP TABLE IF EXISTS PAK_WORK_CENTER_GOLD_TEMP
DROP TABLE IF EXISTS PAK_MATERIAL_GOLD_TEMP
DROP TABLE IF EXISTS PAK_COST_AND_PERCENTAGE_GOLD_TEMP
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_4
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_3
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_2
DROP TABLE IF EXISTS PAK_CSH_CONNECTION_TEMP_1
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP_3
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP_2
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP_1
DROP TABLE IF EXISTS PAK_CALCULATION_GOLD_TEMP
DROP TABLE IF EXISTS PAK_PRICE_GOLD_TEMP_1
DROP TABLE IF EXISTS PAK_PRICE_GOLD_TEMP
DROP TABLE IF EXISTS PAK_DOWNTIME_GOLD_TEMP
DROP TABLE IF EXISTS PAK_CONVERTOR_GOLD_TEMP
DROP TABLE IF EXISTS PAK_FINAL_GOLD_TEMP
DROP TABLE IF EXISTS PAK_REALISATION_GOLD_TEMP




		EXEC [INFO_Warehouse].[dbo].[etlSTOP] @ID=@ID
    END TRY
    BEGIN CATCH
		SET @ERROR_MSG=ERROR_MESSAGE()
		EXEC [INFO_Warehouse].[dbo].[etlERROR] @ID=@ID, @ERROR=@ERROR_MSG;
    END CATCH;
end
GO


