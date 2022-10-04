from ..addict import Dict

skim_names = Dict(
    mf44='auto_am_time',
    mf45='auto_am_dist',
    mf46='auto_md_time',
    mf47='auto_md_dist',
    mf76='auto_am_time_hov',
    mf77='auto_am_dist_hov',
)

skim_tags = Dict({v:k for k,v in skim_names.items()})

skim_names.freeze()
skim_tags.freeze()
