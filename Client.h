#pragma once
#ifndef CLIENT_H

#define CLIENT_H

#ifndef client_ty
enum client_ty { resident, visitor, absence };
#endif // !CLIENT_T

#include <iostream>
#include <fstream>
#include <vector>
#include "../Cpp/Routines/C++/RandomNumbers/random.h" //H for house pc, E for laptop, M for office
#include "../Cpp/json.hpp"        

class client
{
	public:
		client();
		// default constructor constructs an absent client
		client(client_ty type, std::vector<double> means, 
			std::vector<double> sds, double mins[], std::vector<double> rew, 
			std::string _species);
		// contructor																													
		~client();
		// destructor not really necessary
		void printClientData(std::ofstream &learnSeries);
		// prints data from the client into the learning trial
		void rebirth(client_ty type, std::vector<double> means, 
			std::vector<double> sds,	double mins[], 
			std::vector<double> rew, std::string _species);		
		// Function to reset private variables in an individual
		client_ty mytype;
		// The type of the client
		int numFeat;
		double featQuant[3];
		double reward;
		std::string species;
		// Amount of reward obtained by a cleaner if it were to clean the client
		/*double heigth;																							// Heigth of the client
		double length;																								// Length of the client
		double mainRed;																								// Red intensity in the main colour of the client (RGB model)
		double mainGreen;																							// Green intensity in the main colour of the client (RGB model)
		double mainBlue;																							// Blue intensity in the main colour of the client (RGB model)
		double secRed;																								// Red intensity in the secondary colour of the client (RGB model)
		double secGreen;																							// Green intensity in the secondary colour of the client (RGB model)
		double secBlue;																								// Blue intensity in the secondary colour of the client (RGB model)
		bool secondCol;																								// Is there a second colour on the client
		bool stripes;																								// are there stripes on the client
		bool dots;																									// are there dots on the client
		*/
};

client::client()
{
	mytype = absence;
	for (size_t i = 0; i < 3; i++) {
		featQuant[i] = 0;
	}
	reward = 0;
	numFeat = 3;
	species = "NA";
	//heigth = 0, length = 0, mainRed = 0, mainGreen = 0, mainBlue = 0, stripes = 0, dots = 0, reward = 0;
	//secondCol = 0, secRed = 0, secGreen = 0, secBlue = 0;
}

client::client(client_ty type, std::vector<double> means, std::vector<double> sds,
	double mins[], std::vector<double> rew, std::string _species) {
	/*,double mMainRGB[], double sdMainRGB[], double &pSecCol,
	double mSecRGB[], double sdSecRGB[], double &pStripes, double &pDots, double minHL[])*/
	mytype = type;
	numFeat = 3;
	species = _species;
	if (mytype == absence) {
		for (size_t i = 0; i < 3; i++) {
			featQuant[i] = 0;
			reward = 0;
		}
	}
	else {
		for (size_t i = 0; i < 3; i++) {
			featQuant[i] = rnd::normal(means[i], sds[i]);
			clip_range(featQuant[i], 0, 100); 
		}
		/*heigth = rnd::normal(msdHeight[0], msdHeight[1]), length = rnd::normal(msdLength[0], msdLength[1]), mainRed = rnd::normal(mMainRGB[0],sdMainRGB[0]);
		mainGreen = rnd::normal(mMainRGB[1],sdMainRGB[1]), mainBlue = rnd::normal(mMainRGB[2],sdMainRGB[2]), stripes = rnd::bernoulli(pStripes), dots = rnd::bernoulli(pDots);
		secondCol = rnd::bernoulli(pSecCol), reward = 0;*/
		//if (!featBool[0]) { 
		//	for (size_t i = 5; i < 8; i++) { featQuant[i] = 0; }
		//	//secRed = rnd::normal(mSecRGB[0],sdSecRGB[0]), secGreen = rnd::normal(mSecRGB[1],sdSecRGB[1]), secBlue = rnd::normal(mSecRGB[2], sdSecRGB[2]);	
		//	//clip_range(secRed, 0, 255), clip_range(secGreen, 0, 255), clip_range(secBlue, 0, 255);
		//}
		if (rew[1] > 0) {
			reward = (featQuant[0]*2/100)+rnd::normal(0, rew[1]);
			clip_low(reward, 0);
		}
		else reward = rew[0];
	}
}

void client::rebirth(client_ty type=absence, 
	std::vector<double> means = std::vector<double>(),
	std::vector<double> sds = std::vector<double>(), 
	double mins[]=0,
	std::vector<double> rew = { 0,0 }, std::string _species = "NA") {
		mytype = type;
		species = _species;
		if (mytype == absence) {
			for (size_t i = 0; i < 3; i++) {
				featQuant[i] = 0;
			}
		}
		else {
		for (size_t i = 0; i < 3; i++) {
			featQuant[i] = rnd::normal(means[i], sds[i]);
			clip_range(featQuant[i], 0, 255); 
		}
		//if (!featBool[0]) {
		//	for (size_t i = 5; i < 8; i++) { featQuant[i] = 0; }
		//	//secRed = rnd::normal(mSecRGB[0],sdSecRGB[0]), secGreen = rnd::normal(mSecRGB[1],sdSecRGB[1]), secBlue = rnd::normal(mSecRGB[2], sdSecRGB[2]);	
		//	//clip_range(secRed, 0, 255), clip_range(secGreen, 0, 255), clip_range(secBlue, 0, 255);
		//}
		if (rew[1] > 0) {
			reward = (featQuant[0] * 2 / 100) + rnd::normal(0, rew[1]);
			clip_low(reward, 0);
		}
		else reward = rew[0];
	}
}

void client::printClientData(std::ofstream &learnSeries) {
	// prints data from the client into the learning trial
	learnSeries << mytype << '\t';
	learnSeries << species << '\t';
	for (size_t i = 0; i < 3; i++) {
		learnSeries << featQuant[i] << '\t';
		if (featQuant[i] > 100) {
			std::cout << i << '\t' << featQuant[i];
		}
	}
	/*for (size_t i = 0; i < 3; i++) {
		learnSeries << featBool[i] << '\t';
	}*/
	/*learnSeries << mytype << '\t' << heigth << '\t' << length << '\t' << mainRed << '\t' << mainGreen << '\t' << mainBlue << '\t';
	learnSeries << secondCol << '\t' << secRed << '\t' << secGreen << '\t' << secBlue << '\t' << stripes << '\t' << dots << '\t';*/
}

client::~client() {}																								// Destructor

#endif // !CLIENT_H
