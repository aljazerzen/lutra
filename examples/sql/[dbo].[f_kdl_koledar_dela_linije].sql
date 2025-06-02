/****** Object:  View [dbo].[f_kdl_koledar_dela_linije]    Script Date: 19/05/2025 12:10:11 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- Auto Generated (Do not modify) 8B5179E072DD75D39E65823DD724158CDC0A08868A871BA97B7059274D153E19
CREATE VIEW [dbo].[f_kdl_koledar_dela_linije] AS


WITH CTE AS (

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'ALI') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'OB4-ALIP'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'BARSI1') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'BARS1-OPREMA'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'BARSI2') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'BARS2- PRELIV SISTEM'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'PAKIRNICA') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'PAK-OSN'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'POLO') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POLO-OPREMA'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAF 1') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAF-LINIJA-1'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAF 2') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAF-LINIJA-2'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAF 3') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAF-LINIJA-3'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAF 4') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAF-LINIJA-4'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAF 5') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAF-LINIJA-5'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'ROC') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'OB4-ROC'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TF1') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TF1'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TF2') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TF2'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TF3') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TF3'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TF4') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'REMONT TF4'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'THOY') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'OB2-TEHNOHOY'
            AND [CAE Capacity Type Description] = 'Proizvodna celica'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TL2') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TL2'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TL3') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TL3'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TL4') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TL4'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TL5') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TL5'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TL6') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TL6'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'TL7') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'POSKUS-TL7'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'WMF1') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'OB4-WMF1'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'WMF2') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'OB4-WMF2'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'KAL') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'DOD-DELO-COK-KAL'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'MATURACIJA') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'MAT-DO-OPREMA'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'PASTER') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'CEPI'
            AND [CAE Capacity Type Description] = 'Proizvodna celica'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'PILOTNA') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'PIL-BARSI'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'PROI-SPL') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'BUFFER MIZE'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAFINACIJA') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAF-BAZENI'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]

    UNION

    SELECT
        CAST(D.[DAT Date] AS DATE) as datum,
        ISNULL(F.[CAE Work Center Group Code], 'RAZ-DEL') AS linija,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 0
            ELSE CAST(ISNULL(SUM(F.[CAE Capacity (Effective)]), 0) AS INT)
        END AS ure,
        CASE
            WHEN [SCH Shop Calendar Code] IS NOT NULL THEN 'Dela prost dan'
            WHEN [DAT DayName] = 'Sunday' THEN 'Nedelja'
            WHEN
                [SCH Shop Calendar Code] IS NULL
                AND [DAT DayName] <> 'Sunday'
                AND (SUM(F.[CAE Capacity (Effective)]) < 6
                    OR SUM(F.[CAE Capacity (Effective)]) IS NULL) THEN 'Menjava/remont'
            WHEN SUM(F.[CAE Capacity (Effective)]) >= 6
                THEN CASE [DAT DayName]
                    WHEN 'Monday' THEN 'Ponedeljek'
                    WHEN 'Tuesday' THEN 'Torek'
                    WHEN 'Wednesday' THEN 'Sreda'
                    WHEN 'Thursday' THEN 'Četrtek'
                    WHEN 'Friday' THEN 'Petek'
                    WHEN 'Saturday' THEN 'Sobota'
                    END
        END AS tip_dneva
    FROM d_dat_dates D
        LEFT JOIN f_cae_calendar_entry F
            ON CAST(F.[CAE Date] AS DATE) = CAST(D.[DAT Date] AS DATE)
            AND [CAE No_] = 'RAZ-DEL'
        LEFT JOIN d_sch_shop_calendar_holiday H
            ON D.[DAT Date] = H.[SCH Date]
    GROUP BY
        [DAT Date],
        [CAE Work Center Group Code],
        [SCH Shop Calendar Code],
        [DAT DayName]
)

SELECT
	datum AS [KDL Datum],
	linija as [KDL Linija],
	ure as [KDL Ur na dan],
	tip_dneva as [KDL Tip dneva]
FROM CTE
WHERE YEAR(datum) = 2025
--And month(datum) = 5
--AND linija = 'KAL'
--ORDER BY linija, datum ASC
GO


