#ifndef _GRADE_H_
#define _GRADE_H_
#pragma once
#include <string>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief Technologies representing a grade for each resource.
*
* grade is a that contains technologies that characterized each grade.
*
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

using namespace std;
using namespace xercesc;

class grade
{
private:
	string name; //!< grade name
	int year; //!< year
	double available; //!< amount of grade for each grade
	double extCost; //!< extraction cost of each grade
	double envCost; //!< environmental cost of each grade
	double techCh; //!< technical change for all grades
	double tax; //!< severance tax
	double tCost; //!< total cost
public:
	grade(); //default construtor
	grade( const string nameIn, const int noIn );//constructor
	void clear();
	void initElementalMembers();
	void XMLParse( const DOMNode* tempnode ); // initialize with xml data
	void toXML( ostream& out ) const; // write out xml.
	void toDebugXML( const int period, ostream& out ) const;
	void printGrade() const;
	void cost( const int per );
	double getAvail() const;
	double getCost() const;
	double getExtCost() const;
	double getEnvCost() const;
	void setExtCost( const double ecost );
	string getName() const;
};

#endif // _GRADE_H_