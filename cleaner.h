#pragma once
#ifndef CLEANER_H
#define CLEANER_H
#include "Client.h"

using namespace std;

enum learPar { alphaPar, gammaPar, tauPar };

// Global variables
// Client's characteristics
extern double visitMeans[8];
extern double visitSds[8];
extern double visitProbs[3];
extern double residMeans[8];
extern double residSds[8];
extern double residProbs[3];
extern double mins[2];
extern double ResReward;
extern double VisReward;
extern double boolExtUpdate = 10;


class agent																										// Learning agent
{
public:
	agent();																									// basic contructor
	agent(double alphaI, double gammaI, double tauI);															// constructor providing values for the learning parameters
	virtual ~agent();																							// destructor not really necessary
	void updateClient(double &reward, int &countWeights, int clientId);											// function that updates the value of state-action pairs accosrding to current reward and estimates of future values
	void act(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negativeRew);
	// function where the agent takes the action, gets reward, see new state and chooses future action
	void printIndData(ofstream &learnSeries, int &seed, double &bias);											// prints individual data from the learning process
	double getLearnPar(learPar parameter);																		// function to access the learning parameters
	void checkChoice();																							// Check that the choice taken is among one of the options, otherwise trigger an error
	void rebirth();																								// Function to reset private variables in an individual
	void getExternalOptions(client newOptions[], int &idNewOptions,double &RVbias);									// After unattended clinets leave or stay, get new clients
	void ObtainReward();															
	double softMax(double &value1, double &value2);																// softmax function to take a desicion about the two actions
	double valueClient(int clientid, int &countFeat);															// calculates value contribution of 1 client
	void DPupdate(double &probRes, double &probVis, double &VisProbLeav, double &ResProbLeav, double &biasRV);					// change estimated value according to current reward and estimates of future state-action pair
	int mapOptions(client cleanOptions[], int choice);															// map the option to the DP backup
	client cleanOptionsT[2];																					// current cleaning options time = t
	client cleanOptionsT1[2];																					// future cleaning options  time = t+1																											
	// virtual functions
	virtual double value() = 0;																					// Calculates the value estimation for the current option/options
	virtual void choice(int &StaAct1, int &StaAct2) = 0;
	virtual void updateDerived() = 0;
	virtual void getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward)=0;
	int numEst;																									// Number of estimates characterizing bhavioural options
	//void (agent::*pointGNO)(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward) = NULL;                // C++
protected:
	double featWeights[44];																						// vector of weights to fit the state-action value function. Each corresponds to one feature of the client
	double DPbackup[9];																							// vector with the backed-up values of the state-action pairs using dynamic programming
	int DPid;
	double valuesT[2];																							// value estimated for each one of the available options
	double valuesT1[2];
	int choiceT;																								// current choice 
	int choiceT1;																								// future choice
	double currentReward;																						// reward given by current state action pair
	double cumulReward;																							// Cumulative reward
	int age;
	double negReward;
private:
	double alpha;																								// speed of learning
	double gamma;																								// importance of future rewards
	double tau;																									// level of explorative behaviour. The higher, the less important values is when making decisions
};

// Members of agent class

agent::agent()																									// basic constructor
{
	numEst = 23;
	client noClient = client();
	for (size_t i = 0; i < numEst; i++) { featWeights[i] = 0; }
	alpha = 0.01, gamma = 0.5, tau = 10;																		// Default values
	cleanOptionsT[0] = noClient, cleanOptionsT[1] = noClient, choiceT = 2;
	cleanOptionsT1[0] = noClient, cleanOptionsT1[1] = noClient, choiceT1 = 0;
	valuesT[0] = 0, valuesT[1] = 0, valuesT1[0] = 0, valuesT1[1] = 0;
	currentReward = 0, cumulReward = 0, negReward = 0;
	age = 0;
	DPid = -1;
	for (size_t i = 0; i < 9; i++) { DPbackup[i] = 0; }
}

agent::agent(double alphaI, double gammaI, double tauI)															// parameterized constructor
{
	numEst = 23;
	client noClient = client();
	for (size_t i = 0; i < numEst; i++) { featWeights[i] = 0; }
	//values[4] = 10, values[2] = 10;
	alpha = alphaI, gamma = gammaI, tau = tauI;
	cleanOptionsT[0] = noClient, cleanOptionsT[1] = noClient, choiceT = 2;
	cleanOptionsT1[0] = noClient, cleanOptionsT1[1] = noClient, choiceT1 = 0;  
	valuesT[0] = 0, valuesT[1] = 0, valuesT1[0] = 0, valuesT1[1] = 0;
	currentReward = 0, cumulReward = 0, negReward = 0;
	age = 0;
	DPid = -1;
	for (size_t i = 0; i < 9; i++) { DPbackup[i] = 0; }
}

void agent::rebirth()
{
	age = 0;
	client noClient = client();
	cleanOptionsT[0] = noClient, cleanOptionsT[1] = noClient, choiceT = 2;
	cleanOptionsT1[0] = noClient, cleanOptionsT1[1] = noClient, choiceT1 = 0;
	valuesT[0] = 0, valuesT[1] = 0, valuesT1[0] = 0, valuesT1[1] = 0;
	currentReward = 0;
	cumulReward = 0;
	negReward = 0;
	for (size_t i = 0; i < numEst; i++) { featWeights[i] = 0; }
	DPid = -1;
	for (size_t i = 0; i < 9; i++) { DPbackup[i] = 0; }
	//values[4] = 10, values[2] = 10;
}

agent::~agent() {}																								// Destructor

void agent::checkChoice()
{
	if (choiceT>1)
	{
		error("agent::act", "choice is not among the options");
	}
}

double agent::getLearnPar(learPar parameter)
{
	if (parameter == alphaPar) { return alpha; }
	else if (parameter == gammaPar) { return gamma; }
	else { return tau; }
}

void agent::ObtainReward()
{
	currentReward = cleanOptionsT[choiceT].reward;
	cumulReward += cleanOptionsT[choiceT].reward;
}

void agent::getExternalOptions(client newOptions[], int &idNewOptions, double &biasRV)
{
	if (rnd::uniform() < biasRV)
	{
		if (cleanOptionsT1[0].mytype == absence)																			// complete the new state with either 1 or two new clients coming from the environment
		{
			cleanOptionsT1[0].rebirth(resident, residMeans, residSds, mins, residProbs,ResReward);
			cleanOptionsT1[1].rebirth(visitor, visitMeans, visitSds, mins, visitProbs,VisReward);
		}
		else if (cleanOptionsT1[0].mytype == resident)
		{
			cleanOptionsT1[1].rebirth(visitor, visitMeans, visitSds, mins, visitProbs,VisReward);
		}
		else
		{
			cleanOptionsT1[1].rebirth(resident, residMeans, residSds, mins, residProbs,ResReward);
		}
	}
	else
	{
		if (cleanOptionsT1[0].mytype == absence)																			// complete the new state with either 1 or two new clients coming from the environment
		{
			cleanOptionsT1[0] = newOptions[idNewOptions], ++idNewOptions;
			cleanOptionsT1[1] = newOptions[idNewOptions], ++idNewOptions;
		}
		else { cleanOptionsT1[1] = newOptions[idNewOptions], ++idNewOptions; }
	}
}


void agent::act(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negativeRew)// taking action, obatining reward, seeing new state, choosing future action
{
	int StaAct1, StaAct2;
	++age;																										// new time step
	cleanOptionsT[0] = cleanOptionsT1[0], cleanOptionsT[1] = cleanOptionsT1[1];									// Future state becomes current state
	choiceT = choiceT1;																							// Future action becomes current action
	valuesT[0] = valuesT1[0], valuesT[1] = valuesT1[1];
	checkChoice();																								// Check that the choice is among the options
	cleanOptionsT1[0].rebirth(absence, residMeans, residSds, mins, residProbs,biasRV);
	cleanOptionsT1[1].rebirth(absence, residMeans, residSds, mins, residProbs,biasRV);							// Future state is unknown
	choiceT1 = 2;
	ObtainReward();
	getNewOptions(newOptions, idNewOptions, VisProbLeav, ResProbLeav, biasRV, negativeRew);
	choiceT1 = 0;																								// Look into the value of state action pair if option 1 is chosen
	valuesT1[0] = value();
	//StaAct1 = mapOptions(cleanOptionsT1, choiceT1);
	choiceT1 = 1;																								// Look into the value of state action pair if option 1 is chosen
	valuesT1[1] = value();
	//StaAct2 = mapOptions(cleanOptionsT1, choiceT1);
	choice(StaAct1, StaAct2);
}
/*void agent::update(double &reward)																				// change estimated value according to current reward and estimates of future state-action pair
{
	for (size_t i = 0; i < (numEst-1)/2;i++)
	{
		if (i<8)
		{
			featWeights[i] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[0].featQuant[i];
			featWeights[i+ (numEst - 1) / 2] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[1].featQuant[i];
		}
		else
		{
			featWeights[i] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[0].featBool[i-8];
			featWeights[i + (numEst - 1) / 2] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[1].featBool[i-8];
		}
		
	}
	featWeights[22] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*choiceT;	
}*/
void agent::updateClient(double &reward, int &countWeights, int clientId)																				// change estimated value according to current reward and estimates of future state-action pair
{
	for (size_t i = 0; i < cleanOptionsT[clientId].numFeat[0] + cleanOptionsT[clientId].numFeat[1]; i++)
	{
		if (i<cleanOptionsT[clientId].numFeat[0])
		{
			featWeights[countWeights] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[clientId].featQuant[i];
			++countWeights;
		}
		else
		{
			featWeights[countWeights] += boolExtUpdate*alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[clientId].featBool[i - cleanOptionsT[clientId].numFeat[0]];
			++countWeights;
		}

	}
}

void agent::printIndData(ofstream &learnSeries, int &seed,double &bias)
{
	learnSeries << seed << '\t' << age << '\t';
	//cout << seed << '\t' << age << '\t';
	learnSeries << alpha << '\t' << gamma << '\t' << tau << '\t';
	learnSeries << bias << '\t';
	learnSeries << currentReward << '\t' << cumulReward << '\t' << negReward << '\t';
	//cout << currentReward << '\t' << cumulReward << '\t';
	learnSeries << valuesT[choiceT] << '\t';
	cleanOptionsT[choiceT].printClientData(learnSeries);
	if (choiceT == 0) 
	{	
		learnSeries << valuesT[1] << '\t';
		cleanOptionsT[1].printClientData(learnSeries);	}
	else 
	{ 
		learnSeries << valuesT[0] << '\t';
		cleanOptionsT[0].printClientData(learnSeries); 
	}
	learnSeries << DPbackup[DPid] << '\t';
	for (size_t j = 0; j < numEst; j++)
	{
		learnSeries << featWeights[j] << '\t';
		//cout << values[j] << '\t';
	}
	learnSeries << endl;
	//cout << endl;
}

double agent::softMax(double &value1, double &value2)
{
	double prob1 = (exp(value1 / tau)) / (exp(value1 / tau) + exp(value2 / tau));						// Calculate probability of chosing option 1
	return(prob1);
}

double agent::valueClient(int clientid, int &countFeat)
{
	double temp = 0;
	for (size_t i = 0; i < cleanOptionsT1[clientid].numFeat[0] + cleanOptionsT1[clientid].numFeat[1]; i++)
	{
		if (i < cleanOptionsT1[clientid].numFeat[0])
		{
			temp += cleanOptionsT1[clientid].featQuant[i] * featWeights[countFeat],++countFeat;
		}
		else
		{
			temp += cleanOptionsT1[clientid].featBool[i - cleanOptionsT1[clientid].numFeat[0]] * featWeights[countFeat], ++countFeat;
		}
	}
	return(temp);
}

int agent::mapOptions(client cleanOptions[], int choice)
{
	int stateAction;
	if (cleanOptions[0].mytype == absence || cleanOptions[1].mytype == absence)									// One of the options is empty
	{
		if (cleanOptions[0].mytype == resident || cleanOptions[1].mytype == resident)							// the other one is a resident
		{
			if (cleanOptions[choice].mytype == resident) { stateAction = 4; } // State = R0 , action = R		
			else { stateAction = 5; }					// State = R0 , action = 0		
		}
		else if (cleanOptions[0].mytype == visitor || cleanOptions[1].mytype == visitor)						// the other one is a resident
		{
			if (cleanOptions[choice].mytype == visitor) { stateAction = 2; } // State = V0 , action = V
			else { stateAction = 3; }				   // State = V0 , action = 0
		}
		else { stateAction = 8; }					   // State = 00 , action = 0		// the other one is empty too
	}
	else if (cleanOptions[0].mytype == resident || cleanOptions[1].mytype == resident)							// Both options have clients and one of them is a resident
	{
		if (cleanOptions[0].mytype == visitor || cleanOptions[1].mytype == visitor)								// the other one is a visitor
		{
			if (cleanOptions[choice].mytype == resident) { stateAction = 1; }// State = RV , action = R		
			else { stateAction = 0; }				   // State = RV , action = V
		}
		else { stateAction = 7; }					   // State = RR , action = R		
	}
	else { stateAction = 6; }						   // State = VV , action = V
	return stateAction;
}
void agent::DPupdate(double &probRes, double &probVis, double &VisProbLeav, double &ResProbLeav, double &biasRV)																								// change estimated value according to current reward and estimates of future state-action pair
{
	double transProb[9];
	DPid = mapOptions(cleanOptionsT, choiceT);
	if (DPid == 0 || DPid == 5 || DPid == 7)
	{
		transProb[0] = softMax(DPbackup[0], DPbackup[1])*(((1 - ResProbLeav)*probVis + ResProbLeav * 2 * probVis*probRes)*(1 - biasRV) + biasRV);
		transProb[1] = softMax(DPbackup[1], DPbackup[0])*(((1 - ResProbLeav)*probVis + ResProbLeav * 2 * probVis*probRes)*(1 - biasRV) + biasRV);
		transProb[2] = softMax(DPbackup[2], DPbackup[3])*ResProbLeav * 2 * probVis*(1 - probRes - probVis)*(1 - biasRV);
		transProb[3] = softMax(DPbackup[3], DPbackup[2])*ResProbLeav * 2 * probVis*(1 - probRes - probVis)*(1 - biasRV);
		transProb[4] = softMax(DPbackup[4], DPbackup[5])*((1 - ResProbLeav)*(1 - probRes - probVis) + ResProbLeav * 2 * (1 - probRes - probVis)*probRes)*(1 - biasRV);
		transProb[5] = softMax(DPbackup[5], DPbackup[4])*((1 - ResProbLeav)*(1 - probRes - probVis) + ResProbLeav * 2 * (1 - probRes - probVis)*probRes)*(1 - biasRV);
		transProb[6] = ResProbLeav*pow(probVis, 2)*(1 - biasRV);
		transProb[7] = ((1 - ResProbLeav)*probRes + ResProbLeav*pow(probRes, 2))*(1 - biasRV);
		transProb[8] = ResProbLeav*pow((1 - probRes - probVis), 2)*(1 - biasRV);
	}
	else if (DPid == 1 || DPid == 3 || DPid == 6)
	{
		transProb[0] = softMax(DPbackup[0], DPbackup[1])*(((1 - VisProbLeav)*probRes + VisProbLeav * 2 * probVis*probRes)*(1 - biasRV) + biasRV);
		transProb[1] = softMax(DPbackup[1], DPbackup[0])*(((1 - VisProbLeav)*probRes + VisProbLeav * 2 * probVis*probRes)*(1 - biasRV) + biasRV);
		transProb[2] = softMax(DPbackup[2], DPbackup[3])*((1 - VisProbLeav)*(1 - probRes - probVis) + VisProbLeav * 2 * probVis*(1 - probRes - probVis))*(1 - biasRV);
		transProb[3] = softMax(DPbackup[3], DPbackup[2])*((1 - VisProbLeav)*(1 - probRes - probVis) + VisProbLeav * 2 * probVis*(1 - probRes - probVis))*(1 - biasRV);
		transProb[4] = softMax(DPbackup[4], DPbackup[5])* VisProbLeav * 2 * (1 - probRes - probVis)*probRes*(1 - biasRV);
		transProb[5] = softMax(DPbackup[5], DPbackup[4])* VisProbLeav * 2 * (1 - probRes - probVis)*probRes*(1 - biasRV);
		transProb[6] = ((1 - VisProbLeav)*probVis + VisProbLeav*pow(probVis, 2))*(1 - biasRV);
		transProb[7] = VisProbLeav*pow(probRes, 2)*(1 - biasRV);
		transProb[8] = VisProbLeav*pow((1 - probRes - probVis), 2)*(1 - biasRV);
	}
	else
	{
		transProb[0] = softMax(DPbackup[0], DPbackup[1]) * (2 * probVis*probRes*(1 - biasRV) + biasRV);
		transProb[1] = softMax(DPbackup[1], DPbackup[0]) * (2 * probVis*probRes*(1 - biasRV) + biasRV);
		transProb[2] = softMax(DPbackup[2], DPbackup[3]) * 2 * probVis*(1 - probRes - probVis)*(1 - biasRV);
		transProb[3] = softMax(DPbackup[3], DPbackup[2]) * 2 * probVis*(1 - probRes - probVis)*(1 - biasRV);
		transProb[4] = softMax(DPbackup[4], DPbackup[5]) * 2 * (1 - probRes - probVis)*probRes*(1 - biasRV);
		transProb[5] = softMax(DPbackup[5], DPbackup[4]) * 2 * (1 - probRes - probVis)*probRes*(1 - biasRV);
		transProb[6] = pow(probVis, 2)*(1 - biasRV);
		transProb[7] = pow(probRes, 2)*(1 - biasRV);
		transProb[8] = pow((1 - probRes - probVis), 2)*(1 - biasRV);
	}
	DPbackup[DPid] = 0;
	for (int i = 0; i < 9; i++)
	{
		DPbackup[DPid] += transProb[i] * (currentReward + getLearnPar(gammaPar)*DPbackup[i]);
	}

}

class StatPosTyp1
	:public agent
{
public:
	StatPosTyp1(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
		{
			numEst = 44;
		}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		double tautemp = getLearnPar(tauPar);
		if (rnd::uniform() < softMax(valuesT1[0],valuesT1[1]))
		{ choiceT1= 0; }
		else { choiceT1 = 1; }
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		if (choiceT)
		{
			countWeights = 22;
		}
		updateClient(currentReward, countWeights, 0);
		updateClient(currentReward, countWeights, 1);
	}
	virtual double value()
	{
		int countFeat = 0;
		if (choiceT1)
		{
			countFeat = 22;
		}
		double client0 = 0;
		double client1 = 0;
		client0 = valueClient(0, countFeat);
		client1 = valueClient(1, countFeat);
		return(client0 + client1);
	}
	virtual void agent::getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward)
	{
		if (choiceT == 0)																							// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
			else if (cleanOptionsT[1].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
		}
		else if (choiceT == 1)
		{
			if (cleanOptionsT[0].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
			else if (cleanOptionsT[0].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
};

class StatPosTyp1Imp2
	:public agent
{
public:
	StatPosTyp1Imp2(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI) 
	{
		numEst = 44;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
		{
			choiceT1 = 0;
		}
		else { choiceT1 = 1; }
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		if (choiceT)
		{
			countWeights = 22;
		}
		updateClient(currentReward, countWeights, 0);
		updateClient(currentReward, countWeights, 1);
	}
	virtual double value()
	{
		int countFeat = 0;
		if (choiceT1)
		{
			countFeat = 22;
		}
		double client0 = 0;
		double client1 = 0;
		client0 = valueClient(0, countFeat);
		client1 = valueClient(1, countFeat);
		return(client0 + client1);
	}
	virtual void agent::getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward)
	{
		if (choiceT == 0)																							// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
			else if (cleanOptionsT[1].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
		}
		else if (choiceT == 1)
		{
			if (cleanOptionsT[0].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
			else if (cleanOptionsT[0].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
};

class StatPosTyp2 :public agent
{
public:
	StatPosTyp2(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
	{
		numEst = 44;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (cleanOptionsT1[0].mytype != absence && cleanOptionsT1[1].mytype != absence)												// if there are no absences, then use desicion rule
		{
			if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
			{
				choiceT1 = 0;
			}
			else { choiceT1 = 1; }
		}
		else if (cleanOptionsT1[0].mytype == absence)																		// if there is an absence, then chose the other option
		{
			choiceT1 = 1;
		}
		else
		{
			choiceT1 = 0;
		}
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		if (choiceT)
		{
			countWeights = 22;
		}
		updateClient(currentReward, countWeights, 0);
		updateClient(currentReward, countWeights, 1);
	}
	virtual double value()
	{
		int countFeat = 0;
		if (choiceT1)
		{
			countFeat = 22;
		}
		double client0 = 0;
		double client1 = 0;
		client0 = valueClient(0, countFeat);
		client1 = valueClient(1, countFeat);
		return(client0 + client1);
	}
	virtual void agent::getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward)
	{
		if (choiceT == 0)																							// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
			else if (cleanOptionsT[1].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
		}
		else if (choiceT == 1)
		{
			if (cleanOptionsT[0].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
			else if (cleanOptionsT[0].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
};

class StatNegTyp1 :public agent
{
public:
	StatNegTyp1(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
	{
		numEst = 44;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
		{
			choiceT1 = 0;
		}
		else { choiceT1 = 1; }
	}
	virtual void getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negativeRew)
	{
		if (choiceT == 0)																			// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }									// if the unttended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[1].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }								// if the unttended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		else
		{
			if (cleanOptionsT[0].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }		// if the unattended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[0].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }	// if the unattended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		if (choiceT)
		{
			countWeights = 22;
		}
		updateClient(negReward, countWeights, 0);
		updateClient(negReward, countWeights, 1);
	}
	virtual double value()
	{
		int countFeat = 0;
		if (choiceT1)
		{
			countFeat = 22;
		}
		double client0 = 0;
		double client1 = 0;
		client0 = valueClient(0, countFeat);
		client1 = valueClient(1, countFeat);
		return(client0 + client1);
	}
};

class StatNegTyp2 :public agent
{
public:
	using agent::agent;
	StatNegTyp2(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI) {
		numEst = 44;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (cleanOptionsT1[0].mytype != absence && cleanOptionsT1[1].mytype != absence)												// if there are no absences, then use desicion rule
		{
			if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
			{
				choiceT1 = 0;
			}
			else { choiceT1 = 1; }
		}
		else if (cleanOptionsT1[0].mytype == absence)																		// if there is an absence, then chose the other option
		{
			choiceT1 = 1;
		}
		else
		{
			choiceT1 = 0;
		}
	}
	virtual void getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negativeRew)
	{
		if (choiceT == 0)																			// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }									// if the unttended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[1].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }	// if the unttended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		else
		{
			if (cleanOptionsT[0].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { 
					cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }		// if the unattended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[0].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }	// if the unattended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		if (choiceT)
		{
			countWeights = 22;
		}
		updateClient(negReward, countWeights, 0);
		updateClient(negReward, countWeights, 1);
	}
	virtual double value()
	{
		int countFeat = 0;
		if (choiceT1)
		{
			countFeat = 22;
		}
		double client0 = 0;
		double client1 = 0;
		client0 = valueClient(0, countFeat);
		client1 = valueClient(1, countFeat);
		return(client0 + client1);
	}
};

class ActPosTy1 :public agent
{
public:
	ActPosTy1(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
	{
		numEst = 11;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
		{
			choiceT1 = 0;
		}
		else { choiceT1 = 1; }
	}
	virtual double value()
	{
		int countFeat = 0;
		return(valueClient(choiceT1,countFeat));
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		updateClient(currentReward, countWeights, choiceT);
	}
	virtual void agent::getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward)
	{
		if (choiceT == 0)																							// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
			else if (cleanOptionsT[1].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
		}
		else if (choiceT == 1)
		{
			if (cleanOptionsT[0].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
			else if (cleanOptionsT[0].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
};

class ActPosTy2 :public agent
{
public:
	ActPosTy2(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
	{
		numEst = 11;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (cleanOptionsT1[0].mytype != absence && cleanOptionsT1[1].mytype != absence)												// if there are no absences, then use desicion rule
		{
			if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
			{
				choiceT1 = 0;
			}
			else { choiceT1 = 1; }
		}
		else if (cleanOptionsT1[0].mytype == absence)																		// if there is an absence, then chose the other option
		{
			choiceT1 = 1;
		}
		else
		{
			choiceT1 = 0;
		}
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		updateClient(currentReward, countWeights, choiceT);
	}
	virtual double value()
	{
		int countFeat = 0;
		return(valueClient(choiceT1,countFeat));
	}
	virtual void agent::getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward)
	{
		if (choiceT == 0)																							// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
			else if (cleanOptionsT[1].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[1];
			}
		}
		else if (choiceT == 1)
		{
			if (cleanOptionsT[0].mytype == resident && rnd::uniform() > ResProbLeav)								// if the unttended client is a resident, it leaves with probability ResPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
			else if (cleanOptionsT[0].mytype == visitor && rnd::uniform() > VisProbLeav)							// if the unttended client is a visitor, it leaves with probability VisPropLeave
			{
				cleanOptionsT1[0] = cleanOptionsT[0];
			}
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
};

class ActNegTy1 :public agent
{
public:
	ActNegTy1(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
	{
		numEst = 11;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
		{
			choiceT1 = 0;
		}
		else { choiceT1 = 1; }
	}
	virtual void getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negativeRew)
	{
		if (choiceT == 0)																			// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { 
					cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }									// if the unttended client is a resident, it leaves with probability ResPropLeave
				else { 	negReward = negativeRew; }
			}
			else if (cleanOptionsT[1].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) 
				{ cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }	// if the unttended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		else
		{
			if (cleanOptionsT[0].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }		// if the unattended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[0].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }	// if the unattended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		updateClient(negReward, countWeights, choiceT);
	}
	virtual double value()
	{
		int countFeat = 0;
		return(valueClient(choiceT1,countFeat));
	}
};

class ActNegTy2 :public agent
{
public:
	ActNegTy2(double alphaI, double gammaI, double tauI)
		:agent(alphaI, gammaI, tauI)
	{
		numEst = 11;
	}
	virtual void choice(int &StaAct1, int &StaAct2)
	{
		if (cleanOptionsT1[0].mytype != absence && cleanOptionsT1[1].mytype != absence)												// if there are no absences, then use desicion rule
		{
			if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))
			{
				choiceT1 = 0;
			}
			else { choiceT1 = 1; }
		}
		else if (cleanOptionsT1[0].mytype == absence)																		// if there is an absence, then chose the other option
		{
			choiceT1 = 1;
		}
		else
		{
			choiceT1 = 0;
		}
	}
	virtual void getNewOptions(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negativeRew)
	{
		if (choiceT == 0)																			// Define the behaviour of the unattended client
		{
			if (cleanOptionsT[1].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0; }									// if the unttended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[1].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[1], negReward = 0;}	// if the unttended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		else
		{
			if (cleanOptionsT[0].mytype == resident)
			{
				if (rnd::uniform() > ResProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }		// if the unattended client is a resident, it leaves with probability ResPropLeave
				else { negReward = negativeRew; }
			}
			else if (cleanOptionsT[0].mytype == visitor)
			{
				if (rnd::uniform() > VisProbLeav) { cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; }	// if the unattended client is a visitor, it leaves with probability VisPropLeave
				else { negReward = negativeRew; }
			}
			else { negReward = 0; }
		}
		getExternalOptions(newOptions, idNewOptions, biasRV);
	}
	virtual void updateDerived()
	{
		int countWeights = 0;
		updateClient(negReward, countWeights, choiceT);
	}
	virtual double value()
	{
		int countFeat = 0;
		return(valueClient(choiceT1,countFeat));
	}
};

#endif // !CLEANER_H