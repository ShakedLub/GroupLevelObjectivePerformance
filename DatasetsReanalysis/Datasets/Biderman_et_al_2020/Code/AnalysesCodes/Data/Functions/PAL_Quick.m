%
%PAL_Quick   Evaluation of Quick Psychometric Function
%
%   syntax: y = PAL_Quick(params, x)
%
%   y = PAL_Quick(params, x), where 'params' contains the four
%   parameters of a Psychometric Funtion (i.e., [alpha beta gamma lambda]),
%   returns the Psychometric Function evaluated at values in 'x'. 'x' is
%   array of any size.
%
%   x = PAL_Quick(params, y, 'Inverse') returns the x-value at 
%   which the Psychometric Function evaluates to y.
%
%   dydx = PAL_Quick(params, x, 'Derivative') returns the
%   derivative (slope of tangent line) of the Psychometric Function
%   evaluated at x.
%
%   'params' need not have four entries. A two element vector will be
%   interpreted as [alpha beta], a three element vector as [alpha beta
%   gamma]. Missing elements in 'params' will be assigned a value of 0.
%
%   This example returns the function value at threshold when gamma 
%   ('guess-rate') and lambda ('lapse-rate') both equal 0:
%       
%   y = PAL_Quick([1 2 0 0], 1)
%
%   y = 0.5
%
%Introduced: Palamedes version 1.6.0 (NP)

function y = PAL_Quick(params,x,varargin)

[alpha beta gamma lambda] = PAL_unpackParamsPF(params);

if ~isempty(varargin)
    if strncmpi(varargin{1}, 'Inverse',3)
        c = (x - gamma)./(1 - gamma - lambda);
        y = alpha.*(-log(1 - c)./log(2)).^(1./beta);    
    end
    if strncmpi(varargin{1}, 'Derivative',3)
        y = (1-gamma-lambda).*2.^(-1*(x./alpha).^beta).*(x./alpha).^(beta-1).*beta./alpha;
    end
else
    y = gamma + (1 - gamma - lambda).*(1 - 2.^(-1*(x./alpha).^beta));
end