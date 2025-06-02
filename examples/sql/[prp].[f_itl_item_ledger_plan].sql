/****** Object:  View [prp].[f_itl_item_ledger_plan]    Script Date: 19/05/2025 11:00:06 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- Auto Generated (Do not modify) AD07CCF2B9CDD22C77A710B09F4B0CE0375B858966BED71E40E9094C6BE31B5E




create view [prp].[f_itl_item_ledger_plan] as

with mix_tk as -- 1. vrstice iz psc, kjer se 'proizvaja' oz. pakira MIX TK
(
	select
		[PSC Half Item No_],
		[PSC MIXTK Item No_],
		[PSC Days of Work],
		[PSC Scheduled Prod. Date],
		'Pakiranje MIX TK' AS [Type],
		1 AS [Type ID],
		'P6' AS [Category],
		[PSC Routing Line]
	from
		[prp].[f_psc_production_schedule] F
	WHERE
		 [PSC MIXTK Item No_] is null
),
polizdelki AS -- 2. vrstice iz psc, kjer je half item polizdelek in ima izpolnjen mix tk number
(
	select
		[PSC Half Item No_],
		[PSC MIXTK Item No_],
		[PSC Days of Work],
		[PSC Scheduled Prod. Date],
		'Proizvodnja polizdelka' AS [Type],
		1 AS [Type ID],
		'Polizdelki' AS [Category],
		[PSC Routing Line]
	from
		[prp].[f_psc_production_schedule] F
	WHERE
		[PSC MIXTK Item No_] is not null
),

-- 3. nove vrstice, ki za tiste dni, ko se proizvaja MIX TK, v minus da polizdelke od tega mix tk (glede na ici tabelo)
polizdelki_poraba 
as
(
	SELECT
		m.[PSC Scheduled Prod. Date], 
		p.[PSC Half Item No_],
		-1 * m.[PSC Days of Work] * I.[ICI Quantity with Scrap] AS [PSC Days of Work],
		'Poraba polizdelka za MIX TK' AS [Type],
		p.[PSC MIXTK Item No_],
		2 AS [Type ID],
		'Polizdelki' AS [Category],
		p.[PSC Routing Line]
	FROM 
		mix_tk m
		LEFT JOIN polizdelki p on m.[PSC Half Item No_] = p.[PSC MIXTK Item No_]
		LEFT JOIN prp.d_ici_ice_cream_ingredients I 
			on I.[ICI Item No] = p.[PSC MIXTK Item No_] 
			AND I.[ICI Ingredient No] = p.[PSC Half Item No_] 
			AND I.[ICI Ingredient No] <> '16%'
),
-- združimo vse te podatke skupaj
_zaloge_iz_plana as(
	select
		[PSC Half Item No_] as item,
		[PSC Days of Work] as quantity,
		[PSC Scheduled Prod. Date] as [date],
		[Type],
		[Type ID],
		[Category],
		null as mix_tk_item,
		[PSC Routing Line]
	from mix_tk

	union

	select
		[PSC Half Item No_] as item,
		[PSC Days of Work] as quantity,
		[PSC Scheduled Prod. Date] as [date],
		[Type],
		[Type ID],
		[Category],
		[PSC MIXTK Item No_] AS mix_tk_item,
		[PSC Routing Line]
	from polizdelki

	union

	select 
		[PSC Half Item No_] as item,
		[PSC Days of Work] as quantity,
		[PSC Scheduled Prod. Date] as [date],
		[Type],
		[Type ID],
		[Category],
		[PSC MIXTK Item No_] AS mix_tk_item,
		[PSC Routing Line]
	from polizdelki_poraba
),
-- pretvorimo v palete
_zaloge_iz_plana_palete as
(
	SELECT
		item,
		CASE 
			WHEN PALETA = 0 THEN 0
			ELSE F.quantity / PALETA
		END AS quantity,
		[date],
		[Type],
		[Type ID],
		[Category],
		mix_tk_item,
		[PSC Routing Line]
	FROM
		_zaloge_iz_plana F
		left join f_iuc_item_unit_of_measure_converter C on F.item = C.[ITM No_]
),
-- začetna vrednost zalog (v osnovnih merskih enotah)
	zaloge_1_1 as
	(
	select
		F.[ITM Key],
		sum([ITL Quantity]) as [ITL Quantity],
		case when [ITM Key] in (select distinct concat('Incom d_o_o_', [PSC Half Item No_]) from prp.f_psc_production_schedule where [PSC MIXTK Item No_] is null)
			then 'P6'
		else 'Polizdelki'
		end as [Category]
	from dbo.f_itl_item_ledger F
	where [ITL Posting Date] <= DATEFROMPARTS(year(getdate()), 01, 01)
		and (F.[ITM Key] LIKE 'Incom d_o_o_16%'
		OR F.[ITM Key] LIKE 'Incom d_o_o_155%'
		OR F.[ITM Key] LIKE 'Incom d_o_o_145%')
		--and F.[ITM Key] = 'Incom d_o_o_15500173'
	group by F.[ITM Key]
),
-- pretvorimo v palete
	zaloge_1_1_palete as
	(
	select
		RIGHT(F.[ITM Key], CHARINDEX('_', REVERSE(F.[ITM Key])) - 1) AS [ITL Item No_],
		CASE 
			WHEN PALETA = 0 THEN 0
			ELSE F.[ITL Quantity] / PALETA
		END AS [ITL Quantity Pallet],
		[Category]
	from zaloge_1_1 F
		left join f_iuc_item_unit_of_measure_converter C on F.[ITM Key] = C.[ITM Key]
	--where F.[ITM Key] = 'Incom d_o_o_15500173'
),
-- plan prodaje (v kartonih)
plan_prodaje as
(
	--SELECT
	--	[ITM Half Item No_] as [PSC Half Item No_],
	--	[ITM Date] as [PSC Sale Date],
	--	[ITM Quantity Karton] as [PSC Item Quantity],
	--	'P6' AS [Category],
	--	NULL AS [PSC Routing Line]
	--FROM prp.f_itm_item_budget_entry
	--WHERE
	--	[ITM MIXTK Item No_] is null
	--	and YEAR([ITM Date]) = YEAR(GETDATE())
	select 
		[PSC Half Item No_],
		SUM([PSC Item Quantity]) as [PSC Item Quantity],
		[PSC Sale Date],
		'P6' AS [Category],
		[PSC Routing Line]
	from [prp].[f_psc_production_schedule]
	where [PSC MIXTK Item No_] is null --and [PSC Half Item No_] = '16003800'
	group by 
		[PSC Half Item No_],
		[PSC Sale Date],
		[PSC Routing Line]
),
-- pretvorimo v palete
plan_prodaje_palete as
(
	select
		[PSC Half Item No_],
		CASE 
			WHEN PALETA = 0 THEN 0
			ELSE F.[PSC Item Quantity] / PALETA
		END AS sales_q_pallet,
		[PSC Sale Date],
		[Category],
		[PSC Routing Line]
	from plan_prodaje F
		left join f_iuc_item_unit_of_measure_converter C on F.[PSC Half Item No_] = C.[ITM No_]
),
-- združimo vse skupaj
_final as (
	SELECT
		item AS [ITL Item No_],
		quantity AS [ITL Quantity PAL],
		[date] AS [ITL Date],
		[Type] AS [ITL Type],
		[Type ID] AS [ITL Type ID],
		[Category] AS [ITL Category],
		mix_tk_item AS [ITL For MIX TK],
		[PSC Routing Line] AS [ITL Routing Line]
	FROM
		_zaloge_iz_plana_palete

	UNION ALL

	SELECT 
		[ITL Item No_],
		[ITL Quantity Pallet] AS [ITL Quantity PAL],
		DATEFROMPARTS(YEAR(GETDATE()), 01, 01) AS [ITL Date],
		'Začetna zaloga' AS [ITL Type],
		0 AS [ITL Type ID],
		[Category] AS [ITL Category],
		NULL AS [ITL For MIX TK],
		null AS [ITL Routing Line]
	FROM
		zaloge_1_1_palete

	UNION ALL

	SELECT
		[PSC Half Item No_] AS [ITL Item No_],
		-1 * sales_q_pallet AS [ITL Quantity PAL],
		[PSC Sale Date] AS [ITL Date],
		'Plan prodaje' AS [ITL Type],
		2 AS [ITL Type ID],
		[Category] AS [ITL Category],
		NULL AS [ITL For MIX TK],
		[PSC Routing Line] AS [ITL Routing Line]
	FROM
		plan_prodaje_palete
)

select * from _final 
--where [ITL Item No_] in ('16003800', '15500173', '15500174') -- ('16003694', '14500141', '14500142')
--and ([ITL For MIX TK] = '16003800' or [ITL For MIX TK]  is null)
--order by [ITL Date] asc, [ITL Item No_]

--select * from _zaloge_iz_plana_palete
--where item in  ('16003800', '15500173', '15500174')
GO


