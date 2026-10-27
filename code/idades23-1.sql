-- tabela com sub23+1 no mês em questão
SELECT DISTINCT
    FISCAL_NUMBER,
    SERIAL_NUMBER AS card_serial_number,
    CARD_PROFILE_ID,
    TO_CHAR(BIRTH_DATE, 'YYYY-MM-DD') AS birth_date
    -- CARD_NUMBER,
    --TO_CHAR(CARD_DATE, 'YYYY-MM-DD') AS card_date,
    --TO_CHAR(PROFILE_DATE, 'YYYY-MM-DD') AS profile_expiration_date
FROM F_AUX_PROFILE_CARDS
WHERE CARD_PROFILE_ID IN (127, 130) -- sub23+1
	AND CARD_DATE < DATE '2026-09-01' -- alterar mês e ano aqui e na seguinte (intervalo mês)
	AND PROFILE_DATE >= DATE '2026-08-01';
	
	
	
-- salvar em input como 'idades23-1_${table}_${timestamp}'