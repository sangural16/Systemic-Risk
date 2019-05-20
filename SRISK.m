% [INPUT]
% lrmes = A vector of floats containing the LRMES values.
% tl_x  = A numeric vector containing the firm total liabilities.
% mc_x  = A numeric vector containing the firm market capitalization.
% l     = A float [0.05,0.20] representing the capital adequacy ratio (optional, default=0.08).
%
% [OUTPUT]
% srisk = A vector of floats containing the SRISK values.

    srisk = calculate_srisk_internal(lrmes,ip_res.tl_x,ip_res.mc_x,ip_res.l);

function srisk = calculate_srisk_internal(lrmes,tl_x,mc_x,l)

    srisk = (l .* tl_x) - ((1 - l) .* (1 - lrmes) .* mc_x);
    srisk(srisk < 0) = 0;

end
