// Simple_Sarsa2.cpp : Defines the entry point for the console application.
//
/*==============================================================================================================
Simple_Sarsa2.cpp
===============================================================================================================

This is the a new version of the algorithm that allows a learning agent to learn to make desicions in the
context of the market experiment in the context of the cleaner wrasse mutualism.
The difference between this implementation and the main algorithm from 'Simple_Sarsa.cpp' is that in here agents
only estimate the state-action pairs related to the market experiment; while originally an agent also estimated
values relates to the silly option of food agaist nothing. A detailed description of the model can be found in
"E:\Dropbox\Neuchatel\Cleaners_learning_model\\model_description_06_2_2017.pdf".
The model uses the Sarsa algorithm, from the Time Difference (TD) methods from reinforcement learning, to teach
an agent to solve the market expertiment that cleaners face in experimental trials. In the market experiment
individuals are offered two options of clients to clean. This options can be a visitor, a resident, or the
absence of clients. The difference between the two types of clients is that visitors leave the cleaning station
when they are not served, while residents wait; thus, are available in the next time step.




Written by:

Andrés E. Quiñones
Posdoctoral researcher
Behavioural Ecology Group
Institute of Biology
University of Neuchâtel
Switzerland

Start date:
6 February 2017

Last edit date:
15 August 2017


=============================================================================================================*/

#include <stdio.h>
#include <cstdlib>
#include <math.h>
#include <iostream>
#include <fstream>
#include <vector>
// Random number generator
#include "../Cpp/Routines/C++/RandomNumbers/random.h" 
// Classes
#include "cleaner.h"
#include "Client.h"
// Jason parser
#include "../Cpp/json.hpp"        
// Header for reading and using JSON files see https://github.com/nlohmann/json

#define GET_VARIABLE_NAME(Variable) (#Variable)

using namespace std;
using json = nlohmann::json;


double mins[2];

// Functions

rnd::discrete_distribution clientProbs(json param, string clientType) {
	int numSps = param[clientType].size();
	rnd::discrete_distribution SpProb(numSps);
	for (json::iterator itSpClient = param[clientType].begin(); 
		itSpClient != param[clientType].end(); ++itSpClient) {
		SpProb[distance(param[clientType].begin(), itSpClient)] = 
			itSpClient->at("relAbun");
	}
	return (SpProb);
}

void draw(client trainingSet[], json param, 
	rnd::discrete_distribution visitSpProb,
	rnd::discrete_distribution residSpProb) {
	double cumProbs[3] = { param["ResProb"].get<double>(),
		(param["ResProb"].get<double>() + 	
			param["VisProb"].get<double>()),	1 };
	double rndNum;
	json::iterator itSpsVis = param["visitors"].begin();
	json::iterator itSpsRes = param["residents"].begin();
	for (int i = 0; i < param["totRounds"].get<int>() * 2; i++) {
		rndNum = rnd::uniform();
		if (rndNum < cumProbs[0]) {
			string chosenSp = "Sp";
			chosenSp.append(itos(residSpProb.sample() + 1));
			trainingSet[i] = client(resident,param["residents"][chosenSp]["means"],
				param["residents"][chosenSp]["sds"],
				mins, param["residents"][chosenSp]["reward"],chosenSp);
		}
		else if (rndNum < cumProbs[1]) { 
			string chosenSp = "Sp";
			chosenSp.append(itos(visitSpProb.sample()+1));
			trainingSet[i] = client(visitor, param["visitors"][chosenSp]["means"],
				param["visitors"][chosenSp]["sds"],
				mins, param["visitors"][chosenSp]["reward"], chosenSp);
		}
		else { 
			trainingSet[i] = client(); 
		}
	}
}

string create_filename(std::string filename, agent &individual, 
	nlohmann::json param) {
	// name the file with the parameter specifications
	filename.append("alphC");
	filename.append(douts(individual.getLearnPar(alphaCritPar)));
	filename.append("_alphA");
	filename.append(douts(individual.getLearnPar(alphaActPar)));
	filename.append("_gamma");
	filename.append(douts(individual.getLearnPar(gammaPar)));
	filename.append("_neta");
	filename.append(douts(individual.getLearnPar(netaPar)));
	filename.append("_seed");
	filename.append(itos(param["seed"].get<int>()));
	filename.append(".txt");
	return(filename);
}

void initializeIndFile(ofstream &indOutput, agent &learner, 
	nlohmann::json param) {
	std::string namedir = param["folder"];
	//"D:\\quinonesa\\Simulation\\functionAprox\\"; 
	std::string namedirDP = param["folder"];
	//"D:\\quinonesa\\Simulation\\functionAprox\\";
	// "C:\\Users\\quinonesa\\prelimResults\\functionAprox\\";
	// "H:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox\\"; 
	// "E:\\Dropbox\\Neuchatel\\prelimResults\\Set_15\\IndTrain_equVal"
	std::string folder;
	
	folder = typeid(learner).name();
	folder.erase(0, 6).append("_");
	cout << folder << '\t' << learner.getLearnPar(alphaCritPar) << '\t';
	cout << learner.getLearnPar(alphaActPar) << '\t';
	cout << learner.getLearnPar(gammaPar) << '\t';
	cout << learner.getLearnPar(netaPar) << endl;
	namedir.append(folder);
	string IndFile = create_filename(namedir, learner, param);
	indOutput.open(IndFile.c_str());
	
	indOutput << "Training" << '\t' << "Age" << '\t' << "AlphaCri" << '\t';
	indOutput << "AlphaAct" << '\t' << "Gamma" << '\t';
	indOutput << "Neta" << '\t';
	indOutput << "Current.Reward" << '\t';
	indOutput << "Cum.Reward" << '\t' << "Neg.Reward" << '\t';
	indOutput << "value" << '\t' << "preference" << '\t' << "choice" << '\t';
	indOutput << "Type_choice" << '\t' << "Species_choice" << '\t';
	indOutput << "Height_choice" << '\t' << "Length_choice" << '\t';
	indOutput << "redMain_choice" << '\t';
	indOutput << "Type_discard" << '\t' << "Species_discard" << '\t';
	indOutput << "Height_discard" << '\t';
	indOutput << "Length_discard" << '\t' << "redMain_discard" << '\t';
	indOutput << endl;

	/*if (learner.getNfeat() > 11) {
		indOutput << "Height_0_Crit" << '\t' << "Length_0_Crit" << '\t' << "redMain_0_Crit" << '\t';
		indOutput << "greenMain_0_Crit" << '\t' << "blueMain_0_Crit" << '\t' << "redSec_0_Crit" << '\t';
		indOutput << "greenSec_0_Crit" << '\t' << "blueSec_0_Crit" << '\t' << "secCol_0_Crit" << '\t';
		indOutput << "strip_0_Crit" << '\t' << "dots_0_Crit" << '\t' << "Height_1_Crit" << '\t';
		indOutput << "Length_1_Crit" << '\t' << "redMain_1_Crit" << '\t' << "greenMain_1_Crit" << '\t';
		indOutput << "blueMain_1_Crit" << '\t' << "redSec_1_Crit" << '\t' << "greenSec_1_Crit" << '\t';
		indOutput << "blueSec_1_Crit" << '\t' << "secCol_1_Crit" << '\t' << "strip_1_Crit" << '\t';
		indOutput << "dots_1_Crit" << '\t';
		indOutput << "Height_0_Act" << '\t' << "Length_0_Act" << '\t';
		indOutput << "redMain_0_Act" << '\t' << "greenMain_0_Act" << '\t' << "blueMain_0_Act" << '\t';
		indOutput << "redSec_0_Act" << '\t' << "greenSec_0_Act" << '\t' << "blueSec_0_Act" << '\t';
		indOutput << "secCol_0_Act" << '\t' << "strip_0_Act" << '\t' << "dots_0_Act" << '\t';
		indOutput << "Height_1_Act" << '\t' << "Length_1_Act" << '\t' << "redMain_1_Act" << '\t';
		indOutput << "greenMain_1_Act" << '\t' << "blueMain_1_Act" << '\t';
		indOutput << "redSec_1_Act" << '\t' << "greenSec_1_Act" << '\t' << "blueSec_1_Act" << '\t';
		indOutput << "secCol_1_Act" << '\t' << "strip_1_Act" << '\t' << "dots_1_Act" << '\t';
	}
	else {
		indOutput << "Height_Crit" << '\t' << "Length_Crit" << '\t' << "redMain_Crit" << '\t';
		indOutput << "greenMain_Crit" << '\t' << "blueMain_Crit" << '\t' << "redSec_Crit" << '\t';
		indOutput << "greenSec_Crit" << '\t' << "blueSec_Crit" << '\t' << "secCol_Crit" << '\t';
		indOutput << "strip_Crit" << '\t' << "dots_Crit" << '\t';
		indOutput << "Height_Act" << '\t' << "Length_Act" << '\t' << "redMain_Act" << '\t';
		indOutput << "greenMain_Act" << '\t' << "blueMain_Act" << '\t' << "redSec_Act" << '\t';
		indOutput << "greenSec_Act" << '\t' << "blueSec_Act" << '\t' << "secCol_Act" << '\t';
		indOutput << "strip_Act" << '\t' << "dots_Act" << '\t';
	}
	indOutput << endl;*/
}

int _tmain(int argc, char* argv[]) {

	ifstream input(argv[1]);
	if (input.fail()) { cout << "JSON file failed" << endl; }
	json param = nlohmann::json::parse(input);

	//json param;
	//param["totRounds"] = 1000;
	//param["ResProb"] = 0.2;
	//param["VisProb"] = 0.2;
	//param["ResProbLeav"] = 0;
	//param["VisProbLeav"] = 1;
	//param["negativeRew"] = -0.5;
	//param["experiment"] = false;
	//param["inbr"] = 0;
	//param["outbr"] = 0;
	//param["trainingRep"] = 1;
	//param["alphaCrit"] = 0.001;
	//param["alphaAct"] = 0.001;
	//param["printGen"] = 100;
	//param["seed"] = 1;
	//param["gammaRange"] = { 0, 0.8 };
	//param["netaRange"] = { false, true };
	//param["mins"] = { 10, 10 };
	//param["folder"] = "S:/quinonesa/Simulations/functionAprox/ActCrit/test_/";
	//param["visitors"]["Sp1"]["means"] = { 70, 70, 70};
	//param["visitors"]["Sp1"]["sds"] = { 3, 3, 3};
	//param["visitors"]["Sp1"]["probs"] = { 1, 1, 1 };
	//param["visitors"]["Sp1"]["relAbun"] = 1;
	//param["visitors"]["Sp1"]["reward"] = { 1, 0};
	//param["residents"]["Sp1"]["means"] = { 50, 50, 50};
	//param["residents"]["Sp1"]["sds"] = { 3, 3, 3};
	//param["residents"]["Sp1"]["relAbun"] = 1;
	//param["residents"]["Sp1"]["reward"] = { 1, 0};
	
	
	mins[0] = param["mins"][0], mins[1] = param["mins"][1];

	const int numlearn = 1;

	rnd::set_seed(param["seed"].get<int>());
	rnd::discrete_distribution residSpProbs = clientProbs(param, "residents");
	rnd::discrete_distribution visitSpProbs = clientProbs(param, "visitors");

	client *clientSet;
	clientSet = new client[param["totRounds"].get<int>() * 2];
	int idClientSet;

	agent *learners[numlearn];

	cout << param["folder"] << endl;
	for (json::iterator itn = param["netaRange"].begin(); 
		itn != param["netaRange"].end(); ++itn) {
		for (json::iterator itg = param["gammaRange"].begin(); 
			itg != param["gammaRange"].end(); ++itg) {
			ofstream printTest;
			ofstream DPprint;
			learners[0] = new FIATy1(param["alphaCrit"].get<double>(),
				param["alphaAct"].get<double>(),*itg, *itn);
			/*learners[1] = new PIATy2(param["alphaT"].get<double>(),
				*itg, *itt, *itn);*/
			for (int k = 0; k < numlearn; ++k) {
				initializeIndFile(printTest, *learners[k], param);
				for (int i = 0; i < param["trainingRep"].get<int>(); 
					i++) {
					draw(clientSet, param, visitSpProbs,residSpProbs);
					idClientSet = 0;
					for (int j = 0; j < param["totRounds"].get<int>(); j++) {
						learners[k]->act(clientSet, idClientSet, param,
							visitSpProbs, residSpProbs);
						learners[k]->update();
						if (j % param["printGen"].get<int>() == 0) {
							learners[k]->printIndData(printTest, i);
						}
					}
					learners[k]->rebirth();
				}
				printTest.close();
				delete learners[k];
			}
		}
	}

	delete[] clientSet;
	
	/*wait_for_return();*/

	return 0;
}
