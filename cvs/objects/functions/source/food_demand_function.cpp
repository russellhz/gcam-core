/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file food_demand_function.cpp
* \ingroup Objects
* \brief The FoodDemandFunction class source file.
* \author Pralit Patel
* \author Robert Link
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/food_demand_function.h"
#include "functions/include/iinput.h"
#include "functions/include/food_demand_input.h"

using namespace std;

/*!
 * \brief Calculate the price of the parent node form the bottom up.
 * \details In the food demand system the price of the parent is the price of "marerials" which is just
 *          held constant so this method just returns that price unchanged.
 */
double FoodDemandFunction::calcLevelizedCost( const InputSet& aInputs, const std::string& aRegionName,
                          const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma,
                          const IInput* aParentInput ) const
{
    // This is the price of materials which does not to get adjusted by this function.
    return aParentInput->getPrice( aRegionName, aPeriod );
}

/*!
 * \brief Calculate the food demand equations.
 * \details The demand function for each staple and non-staples is given by:
 *          q = A * (x^h(x)) * (w_self^e_self(x)) * (w_cross^e_cross(x))
 *          where:
 *          A: The scale parameter
 *          x: income diveded by price of materials
 *          x^h(x): calculated all together depending on the type of FoodDemandInput, see StaplesFoodDemandInput::calcIncomeTerm,
 *               NonStaplesFoodDemandInput::calcIncomeTerm)
 *          w_i: The price of the food input divided by the price of materials times some scale factor.
 *          e_self: g_self - alpha * f(x)
 *          e_cross: g_cross - alpha_cross * f(x)
 *          g_self: self price elasticity parameter
 *          g_cross: cross price elasticity depending on the type of FoodDemandInput, see StaplesFoodDemandInput::getCrossPriceElasticity,
 *               NonStaplesFoodDemandInput::getCrossPriceElasticity)
 *          alpha: The share of the total budget for the good (note due to the circular dependence on need to share to calculate the share
 *               value used here is trial value generated by the solver)
 *          f(x): the derivative of the income term depending on the type of FoodDemandInput, see StaplesFoodDemandInput::calcIncomeTermDerivative,
 *               NonStaplesFoodDemandInput::calcIncomeTermDerivative)
 *
 * \param aInput The child nodes to calculate demands for.  We are expecting these to be subclasses of FoodDemandInput
 * and should have one for staples and non-staples.
 * \param aRegionName The name of the containing region.
 * \param aPeriod The current model period.
 * \param aPriceAbove The price of the parent node which is the considered the price of materials.
 * \return The "demand" for materials which is really just the residual from the food demands.
 */
double FoodDemandFunction::calcDemand( InputSet& aInput, double income, const std::string& aRegionName,
                       const std::string& sectorName, const double aShutdownCoef, int aPeriod,
                       double capitalStock, double alphaZero, double sigma, double aPriceAbove, const IInput* aParentInput ) const
{
    // the price of the parent node which is the considered the price of materials
    double priceMaterials = aPriceAbove;

    vector<FoodDemandInput*> foodInputs( aInput.size() );
    vector<double> adjPrices( aInput.size() );
    for( size_t i = 0; i < aInput.size(); ++i ) {
        // we are expecting the child nodes to be subclasses of FoodDemandInput
        foodInputs[i] = static_cast<FoodDemandInput*>( aInput[i] );
        
        /*!
         * \pre Child inputs must be subclasses of FoodDemandInput.
         */
        assert( foodInputs[i] );
        
        // calculate the adjusted prices for each food demand type (w)
        adjPrices[i] = foodInputs[i]->getPrice( aRegionName, aPeriod ) / priceMaterials * foodInputs[i]->getPriceScaler();
    }
    
    // calculate the adjusted income (x)
    double adjIncome = foodInputs[ 0 ]->getSubregionalIncome() / priceMaterials;

    // calculate demands
    double demandMaterials = adjIncome;
    double alphaTotal = 0.0;
    vector<double> alphaActual( aInput.size() );
    vector<double> demands( aInput.size() );
    for( size_t i = 0; i < aInput.size(); ++i ) {
        // calculate the first part of the equation: A * x^h(x) (note calcIncomeTerm will
        // calculate all of x^h(x) as there is implicitly a scale term included there)
        double currDemand = foodInputs[i]->getScaleTerm() * foodInputs[i]->calcIncomeTerm( adjIncome );
        // calculate the price terms of the equations MULT_j(w_j ^ e_j(x))
        for( size_t j = 0; j < aInput.size(); ++j ) {
            // if i == j we are doing the self price term, otherwise the cross price term
            currDemand *= pow( adjPrices[j], i == j ?
                              foodInputs[i]->calcSelfPriceExponent( adjIncome, aRegionName, aPeriod ) :
                              foodInputs[i]->calcCrossPriceExponent( foodInputs[j], adjIncome, aRegionName, aPeriod ) );
        }
        demands[i] = currDemand;
        // the demand for materials is just the residual of the food demand:
        // q_m = x - SUM_i(w_i * q_i)
        demandMaterials -= adjPrices[i] * currDemand;
        // calculate what the actual shares ended up being too
        alphaActual[i] = adjPrices[i] * currDemand / adjIncome / foodInputs[i]->getPriceScaler();
        alphaTotal += alphaActual[i];
    }
    
    // Check budget constraint.  If we're spending more than the total income on
    // food, then reduce nonstaples first, followed by staples.  We'll use the
    // actuals for this calculation.
    //double alphat = alphas_actual + alphan_actual;
    double budget = 1.0;        // Fraction of income available for food. We
                                // didn't expermient with changing this in the
                                // original model development.
    
    if(alphaTotal > budget) {
        if(alphaActual[1] < budget) {
            alphaActual[0] = budget - alphaActual[1];
        }
        else {
            alphaActual[0] = 0.0;
            alphaActual[1] = budget;
        }
        // Recalculate quantities based on the new budget fractions
        for( size_t i = 0; i < aInput.size(); ++i ) {
            demands[i] = alphaActual[i] * adjIncome/adjPrices[i] * foodInputs[i]->getPriceScaler();
        }
    }
    
    // Set the final demands and actual shares into the inputs which will
    // take care of updating the values in the marketplace.
    // Note the demands are still in terms of Mcal / person / day at this
    // point, the inputs will take care of converting to Pcal / year as
    // is expected in the supply sectors.
    for( size_t i = 0; i < aInput.size(); ++i ) {
        foodInputs[i]->setPhysicalDemand( demands[i], aRegionName, aPeriod );
        foodInputs[i]->setActualShare( alphaActual[i], aRegionName, aPeriod );
    }

    // Return the demand for materials which isn't actually used in model
    // calculations but will be stored for reporting.
    return demandMaterials;
}

