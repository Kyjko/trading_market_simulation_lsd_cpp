//#define EIGENLIB			// uncomment to use Eigen linear algebra library

#include "fun_head_fast.h"

#define ___NO_EXPORT_CSV___ 1 // 0-ra kell állítani ha exportálni kell az egy darab futtatás utáni adatokat automatikusan
#define ___NO_EXPORT_MASS_CSV___ 1 // 0-ra kell állítani ha exportálni kell a több futtatás utáni adatokat automatikusan

#define MAX_LOOKBACK 10 // maximum árkésleltetés
#define PRICE_START 100 // kezdöár
#define MAX_PRICE 50000 // max ár
#define MAX_NUMBER_OF_AGENTS 6400 // ágensek maximális száma (négyzetszám)

#define AVG_STARTING_M_STD 100
#define AVG_STARTING_V_STD 1



#define NEWOBJECT cur1 = ADDOBJ("Agent"); \
	WRITES(cur1, "M", norm(V("avg_starting_m"), AVG_STARTING_M_STD)); \
	WRITES(cur1, "V", norm(V("avg_starting_v"), AVG_STARTING_V_STD)); \
	WRITES(cur1, "rho_ma", uniform(-1, 1)); \
	WRITES(cur1, "rho_bb", uniform(-1, 1)); \
	WRITES(cur1, "rho_f", uniform(-1, 1)); \
	WRITES(cur1, "gamma", uniform(0, 1)); \
	WRITES(cur1, "k", 1+(int)(uniform(0,1)*(MAX_LOOKBACK-1))); \
	WRITES(cur1, "EMA", VL("P", 1)); \
	WRITES(cur1, "mutation_prob", uniform(0, 0.1)); \
	WRITES(cur1, "sigma_trade", uniform(0, 0.1)); \
	WRITES(cur1, "smoothing", uniform(1.5, 2.5)); \
	WRITES(cur1, "prob_interaction", uniform(0, 1)); \
	v[0]++;

#if ___NO_EXPORT_CSV___ == 0
FILE* ___f = NULL;
FILE* ___f2 = NULL;
FILE* ___f3 = NULL;
FILE* ___f4 = NULL;
FILE* ___f5 = NULL;
FILE* ___f6 = NULL;
FILE* ___f7 = NULL;
FILE* ___f8 = NULL;
#endif

#if ___NO_EXPORT_MASS_CSV___ == 0
float* avg_returns;
int _current_time;
#endif

MODELBEGIN

EQUATION("Init")

#if ___NO_EXPORT_MASS_CSV___ == 0
_current_time = 0;

avg_returns = new float[2000];
if(avg_returns == NULL) {
	plog("[avg_returns -> PANIC!] heap alloc failed\n");
}
#endif

#if ___NO_EXPORT_CSV___ == 0
___f = fopen("price.csv", "w+");
___f2 = fopen("volume.csv", "w+");
___f3 = fopen("number.csv", "w+");
___f4 = fopen("return.csv", "w+");
___f5 = fopen("wealth.csv", "w+");
___f6 = fopen("trader_strat.csv", "w+");
___f7 = fopen("trader_strat_fund.csv", "w+");
___f8 = fopen("fundament.csv", "w+");

if(___f == NULL) {
	plog("[price.csv] ERR\n");
}
if(___f2 == NULL) {
	plog("[volume.csv] ERR\n");
}
if(___f3 == NULL) {
	plog("[number.csv] ERR\n");
}
if(___f4 == NULL) {
	plog("[return.csv] ERR\n");
}
if(___f5 == NULL) {
	plog("[wealth.csv] ERR\n");
}
if(___f6 == NULL) {
	plog("[trader_strat.csv] ERR\n");
}
if(___f7 == NULL) {
	plog("[trader_strat_fund.csv] ERR\n");
}
if(___f8 == NULL) {
	plog("[fundament.csv] ERR\n");
}
#endif

/*
	a szimuláció elején az ár -MAX_LOOKBACK indexig beállítódik véletlenszerüen,
	az ágensek egy gridbe tev?dnek  

*/

cur = SEARCH("Market");
for(int iter_idx = 1; iter_idx <= MAX_LOOKBACK; iter_idx++) {
	WRITELS(cur, "P", norm(PRICE_START, V("sigma_price")), t-iter_idx);
}

int iter_idx = 0;

CYCLES(cur, cur1, "Agent") {
	WRITES(cur1, "X", iter_idx%(int)(sqrt(MAX_NUMBER_OF_AGENTS)));
	WRITES(cur1, "Y", (int)(iter_idx/sqrt(MAX_NUMBER_OF_AGENTS)));
	
	// M, V és B kezd?értékeinek beállítása minden ágensnek (t-1, t-2)
	for(int _iter_idx = 1; _iter_idx <= 2; _iter_idx++) {
		v[0] = norm(VS(cur, "avg_starting_m"), AVG_STARTING_M_STD);
		v[1] = norm(VS(cur, "avg_starting_v"), AVG_STARTING_V_STD);
	
		WRITELS(cur1, "M", v[0], t-1);
		WRITELS(cur1, "V", v[1], t-1);
		WRITELS(cur1, "B", v[0]+v[1], t-1);
	}
	
	iter_idx++;
}

PARAMETER
RESULT(1)


//////////////// <Piac> ////////////////////////////////////////

// Fundamentum egy AR(1) folyamat
EQUATION("F")
v[0] = VL("F", 1) + norm(0, V("sigma_fundament"));

#if ___NO_EXPORT_CSV___ == 0
fprintf(___f8, "%f,", (float)v[0]);
#endif

RESULT(v[0])

EQUATION("P")
/*
	az ár az ágensek aggergált döntései (sum(Trade)) és egy alpha paraméter szorzatának függvényében
	alakul egy Norm(0, sigma_price^2) sztochasztikus fluktuáció mellett.
	
	ha az ár egy MAX_PRICE fölé menne, akkor lekorlátozódik arra a szintre
	az ár nem mehet 0.01 alá (a 0 érték NaN-okat okozna)

*/

v[0] = 0;
CYCLE_SAFE(cur, "Agent") {
	v[0] += VLS(cur, "Trade", 1);
}

//v[0] /= log(VL("N", 1));

v[1] = VL("P", 1) + V("alpha")*v[0] + norm(0, V("sigma_price"));

if(v[1] > MAX_PRICE) v[1] = MAX_PRICE;

#if ___NO_EXPORT_CSV___ == 0
float __res = (float)(max(v[1], 0.01));
fprintf(___f, "%f,", __res);
#endif

RESULT( max(v[1], 0.01) )

EQUATION("N")
/*
	ha egy ágens vagyona 0.01 alá csökkenne, akkor kiszáll
	minden iterációban egy véletlenszerü ágens exit_prob-al kiszáll
	minden iterációban entry_prob valószín?séggel egy új ágens száll be akkor,
	hogy ha az ágensek száma nem lépné túl a MAX_NUMBER_OF_AGENTS-et

*/

int deleted_x = -1, deleted_y = -1;
int deleted_x2 = -1, deleted_y2 = -1;

v[0] = VL("N", 1);

CYCLE_SAFE(cur, "Agent") {
	if(VS(cur, "B") <= 50) {
		// nincs elég pénze, kiszáll
		// csak akkor, ha a törlés után legalább egy ágens lesz még
		if(v[0] > 1) {
			deleted_x = VS(cur, "X");
			deleted_y = VS(cur, "Y");
			DELETE(cur);
			v[0]--;
		}
	}
}

// csak akkor szállhat ki random, ha utána legalább marad még egy ágens
if(RND < V("exit_prob")) {
	if(v[0] > 1) {
		// ha még van ágens
		cur = RNDDRAW_FAIR("Agent");
		// biztonság kedvéért
		if(cur != NULL) {
			deleted_x2 = VS(cur, "X");
			deleted_y2 = VS(cur, "Y");
			DELETE(cur);
			v[0]--;
		}
	}
}

// be tud szállni új ágens a gridbe entry_prob-al
if(RND < V("entry_prob") && v[0] < MAX_NUMBER_OF_AGENTS) {
	NEWOBJECT
	
	if(deleted_x == -1 && deleted_y == -1 && deleted_x2 == -1 && deleted_y2 == -1) {
		// nincs üres hely
		int new_x = (int)v[0]%(int)(sqrt(MAX_NUMBER_OF_AGENTS));
		int new_y = (int)((int)v[0]/sqrt(MAX_NUMBER_OF_AGENTS));
		WRITES(cur1, "X", new_x);
		WRITES(cur1, "Y", new_y);
	} else if(deleted_x != -1 && deleted_y != -1 && deleted_x2 == -1 && deleted_y2 == -1) {
		WRITES(cur1, "X", deleted_x);
		WRITES(cur1, "Y", deleted_y);
	} else if(deleted_x == -1 && deleted_y == -1 && deleted_x2 != -1 && deleted_y2 != -1) {
		WRITES(cur1, "X", deleted_x2);
		WRITES(cur1, "Y", deleted_y2);
	} else {
		// mindkét hely üres, random választ
		if(RND < 0.5) {
			WRITES(cur1, "X", deleted_x);
			WRITES(cur1, "Y", deleted_y);
		} else {
			WRITES(cur1, "X", deleted_x2);
			WRITES(cur1, "Y", deleted_y2);
		}
	}
}

#if ___NO_EXPORT_CSV___ == 0
int ___res = (int)v[0];
fprintf(___f3, "%d,", ___res);

///// átlagos ágens-strat
float avg_strat = 0;
float avg_strat_f = 0;

CYCLE_SAFE(cur, "Agent") {
	avg_strat += VS(cur, "rho_ma");
	avg_strat_f += VS(cur, "rho_f");
}

avg_strat /= v[0];
avg_strat_f /= v[0];

fprintf(___f6, "%f,", avg_strat);
fprintf(___f7, "%f,", avg_strat_f);
#endif

RESULT(v[0])

EQUATION("Mutation")
/*
	minden iterációban az ágensek mutálódnak egy mutation_prob valószín?séggel

*/

CYCLE_SAFE(cur, "Agent") {
	if(RND < VS(cur, "mutation_prob")) {
		INCRS(cur, "rho_ma", uniform(0, V("mutation_level"))-V("mutation_level")/2);
		INCRS(cur, "rho_bb", uniform(0, V("mutation_level"))-V("mutation_level")/2);
		INCRS(cur, "rho_f", uniform(0, V("mutation_level"))-V("mutation_level")/2);
		INCRS(cur, "gamma", uniform(0, V("mutation_level"))-V("mutation_level")/2);
		INCRS(cur, "smoothing", uniform(0, V("mutation_level"))-V("mutation_level")/2);
			
		if(VS(cur, "k")+1 <= MAX_LOOKBACK && VS(cur, "k")-1 > 0) {
			if(RND < 0.5) {
				INCRS(cur, "k", 1);
			} else {
				INCRS(cur, "k", -1);
			}
		}
	}
}

PARAMETER
RESULT(0)

EQUATION("Total_Trade")
/*
	a teljes Trade mennyiség

*/
v[0] = 0;
CYCLE_SAFE(cur, "Agent") {
	v[0] += abs(VS(cur, "Trade"));
}

#if ___NO_EXPORT_CSV___ == 0
float ___res = (float)v[0];
fprintf(___f2, "%f,", ___res);
#endif

RESULT(v[0])

EQUATION("Avg_B")
/*
	az átlagos vagyona egy ágensnek

*/
v[0] = 0;
CYCLE_SAFE(cur, "Agent") {
	v[0] += VS(cur, "B");	
}
v[0] /= V("N");

#if ___NO_EXPORT_CSV___ == 0
float ___res = (float)v[0];
fprintf(___f5, "%f,", ___res);
#endif

RESULT(v[0])

EQUATION("Avg_V")
/*
	az átlagos részvények száma egy ágensnél

*/
v[0] = 0;
CYCLE_SAFE(cur, "Agent") {
	v[0] += VS(cur, "V");	
}
v[0] /= V("N");
RESULT(v[0])


EQUATION("Avg_M")
/*
	az átlagos likvid eszköze egy ágensnek

*/
v[0] = 0;
CYCLE_SAFE(cur, "Agent") {
	v[0] += VS(cur, "M");	
}
v[0] /= V("N");
RESULT(v[0])

EQUATION("Avg_Return")
/*
	az egyperiódusos return (%)
*/
v[0] = (V("P") - VL("P", 1))/VL("P", 1);

#if ___NO_EXPORT_CSV___ == 0
float ___res = (float)v[0];
fprintf(___f4, "%f,", ___res);
#endif

#if ___NO_EXPORT_MASS_CSV___ == 0
avg_returns[_current_time] = (float)v[0];

if(_current_time == 2000-1) {
	// szimuláció vége, kiírni fileba
	char _file_name[30];
	sprintf(_file_name, "Sim1_%d.csv", cur_sim);
	
	FILE* ___avg_returns = fopen(_file_name, "w+");
	if(___avg_returns == NULL) {
		plog("[Sim1/Sim1_%d] ERR\n", cur_sim);
	}
	for(int iter_idx = 0; iter_idx < 2000; iter_idx++) {
		fprintf(___avg_returns, "%f,", avg_returns[iter_idx]);
	}
	fclose(___avg_returns);
	delete[] avg_returns;
	
	_current_time = 0;
} else {
	_current_time++;
}
#endif

RESULT(v[0])
/////////////////// </Piac> //////////////////////////////////////

////////////////// <Ágens> ///////////////////////////////////////

// exponenciálisan simított mozgóátlag (ágens-specifikus smoothing mellett)
EQUATION("EMA")
v[0] = VL("EMA", 1)*(1-V("smoothing")/(1+V("k"))) + V("P")*(V("smoothing")/(1+V("k")));
RESULT(v[0])

EQUATION("prob_trade")
/*
	az ágens prob_trade-el tradel minden idöperiódusban:
		prob_trade függ a profittól: 
			prob_trade	+=	Norm(0.15, 0.01)	ha B_{t-1} - B_{t-2} > 0
						-=	Norm(0.1, 0.01)		egyébként

*/

v[0] = VL("B", 2);
v[1] = VL("B", 1);
v[2] = VL("prob_trade", 1);

if(v[1]-v[0] > 0) {
	v[2] += norm(0.15, 0.01);
} else {
	v[2] += norm(-0.1, 0.01);
}

if(v[2] > 1) v[2] = 1;
if(v[2] < 0) v[2] = 0;

RESULT(v[2])

EQUATION("M")
/*
	likvid eszköz (cash)
*/
v[0] = VL("M", 1) - VL("Trade", 1)*VL("P", 1);

RESULT(v[0])

EQUATION("V")
/*
	az ágensnél lév? részvények száma
*/
v[0] = VL("V", 1) + VL("Trade", 1);

RESULT(v[0])

EQUATION("Q")
RESULT( V("V")*V("P") )

EQUATION("B")
v[0] = V("M") + V("V")*V("P");
RESULT(v[0])

EQUATION("Return")
/*
	Egyperiódusú return

*/
v[0] = V("B") - VL("B", 1);
RESULT(v[0])

EQUATION("Trade")
/*
	az ágens vagyona: B = M + Q, ahol
		M : likvid eszköz (cash)
		Q : a kereskedett pénzügyi termék piaci értéke
			Q = P * V, ahol
			V = a nála lév? részvények száma
			V_t = V_{t-1} + Trade_t

	az ágens több szempontot is figyelembe vesz döntéshozatalánál (Trade kiszámításánál):
		1) P_t - MA(k)_t -> (moving average indicator) 
		2) EMA_t = P_t * (smoothing/(1+k)) + EMA_{t-1}*(1-smoothing/(1+k))  -> (exponenciálisan simított MA)
		3) STD(P, k) (árvolatilitás k-ablakban)
		4) BB_t = EMA_t +- STD(P, k) -> (egyszerüsített Bollinger Bands)
		
		==
		
		a döntés: D (érték):
			D < 0: SELL
			D = 0: NO TRADE
			D > 0: BUY
		
		rho_ma-parametrizált döntés:
		D += (P_t - MA(k)_t) * rho_ma
		
		rho_bb-parametrizált döntés:
		ha P_t > BB_t+ vagy P_t < BB_t-:
			D += (P_t - BB_t+) * rho_bb
		vagy
			D += (P_t - BB_t-) * rho_bb
			
		rho_f-parametrizált döntés:
		D += (P_t - F_t) * rho_f
		
	az ágens likviditási paramétere gamma:
		gamma a minimális elfogadható M/B érték
		
	az ágens a végsö döntését (D+Norm(0, sigma_trade^2)) * (kockázati érték) alapján állapítja meg
	
	az ágens B-függ? prob_trade valószínöséggel tradel az adott interációban,
		ha venni akar (Trade > 0), akkor csak úgy teheti meg, ha van elég M-je, illetve ha ezzel a vétellel M/B
		értéke nem megy gamma alá
		ha eladni akar (Trade < 0), akkor maximum annyit adhat el, amennyi V-je van
		
*/

v[0] = 0;
for(int iter_idx = 1; iter_idx < V("k"); iter_idx++) {
	v[0] += VL("P", iter_idx);
}
v[0] += V("P");
v[0] /= V("k"); // MA(k)_t
v[1] = V("P") - v[0]; // P_t - MA(k)_t

v[2] = 0;
for(int iter_idx = 1; iter_idx < V("k"); iter_idx++) {
	v[2] += pow(VL("P", iter_idx)-v[0], 2);
}
v[2] += pow(V("P")-v[0], 2);
v[2] /= V("k");
v[2] = sqrt(v[2]); // STD(P, k)

v[3] = V("EMA") + v[2]; // BB+
v[4] = V("EMA") - v[2]; // BB-
if(v[4] < 0.01) v[4] = 0.01; // a BB- nem mehet negatívba 

v[5] = 0; // BB
if(V("P") > v[3]) {
	v[5] = V("P") - v[3];
}
if(V("P") < v[4]) {
	v[5] = V("P") - v[4];
}

v[6] = V("F") - V("P"); // F_t - P_t

v[7] = V("rho_ma")*v[1] + V("rho_bb")*v[5] + V("rho_f")*v[6]; // D

v[7] += norm(0, V("sigma_trade")); 

if(v[7] > 0) {
	// ha venni akar
	v[8] = v[7]*V("P");
	if(v[8] > V("M")) {
		// nincs elég likviditása
		v[7] = 0;
	}
} else if(v[7] < 0) {
	// ha eladni akar
	if(abs(v[7]) > V("V")) {
		// maximum a nála lév? részvények mennyiségét adhatja el
		v[7] = V("V");
	}
}
// ellenörzi, hogy nem lenne-e túl kevés likviditása a trade végrehajtásával
v[9] = (V("M")-V("P")*v[7])/(V("M")-V("P")*v[7] + (v[7]+V("V"))*V("P"));
if(v[9] < V("gamma")) {
	// ha túl kevés lenne a likviditása, nem vesz
	if(v[7] > 0) {
		v[7] = 0;
	}
}

if(RND >= V("prob_trade")) {
	v[7] = 0;
	
}

RESULT(v[7])

EQUATION("Interaction")
/*
	interakció

*/

v[0] = 0;
CYCLE_SAFE(cur, "Agent") {
	if(p != cur) {
		if(abs(V("X") - VS(cur, "X") + V("Y") - VS(cur, "Y")) <= 1) {
			// cur az ágens szomszédságában van
			v[1] = VLS(cur, "B", 1) - VLS(cur, "B", 2);
			if(v[1] > v[0]) v[0] = v[1];	
		}
	}
}
CYCLE_SAFE(cur, "Agent") {
	if(VLS(cur, "B", 1) - VLS(cur, "B", 2) == v[0] && abs(V("X") - VS(cur, "X") + V("Y") - VS(cur, "Y")) <= 1) {
		if(RND < V("prob_interaction")) {
			// tanul cur-töl
			WRITE("rho_ma", VS(cur, "rho_ma")+norm(0, V("sigma_learning")));
			WRITE("rho_bb", VS(cur, "rho_bb")+norm(0, V("sigma_learning")));
			WRITE("rho_f", VS(cur, "rho_f")+norm(0, V("sigma_learning")));
			WRITE("smoothing", VS(cur, "smoothing")+norm(0, V("sigma_learning")));
			WRITE("k", VS(cur, "k"));
		}
		
	}
	
}

PARAMETER
RESULT(0)

MODELEND


void close_sim( void ) {

#if ___NO_EXPORT_CSV___ == 0
	fclose(___f);
	fclose(___f2);
	fclose(___f3);
	fclose(___f4);
	fclose(___f5);
	fclose(___f6);
	fclose(___f7);
	fclose(___f8);
#endif
}

