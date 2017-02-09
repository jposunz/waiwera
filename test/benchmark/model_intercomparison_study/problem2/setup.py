from t2data_json import *
from t2incons import *
from t2thermo import cowat
from builtins import zip
from math import ceil
import json
import os

model_dir = './run'
orig_dir = os.getcwd()
if not os.path.isdir(model_dir): os.makedirs(model_dir)
os.chdir(model_dir)

model_name = 'problem2'
dr = np.concatenate((np.array([0.3, 0.4, 0.6]),
                     0.6 * np.logspace(1, 29, 30, base = 1.28)))
thickness = 100.
dz = np.array([thickness])

geo = mulgrid().rectangular(dr, [1.], dz, atmos_type = 2)
geo.write('g' + model_name + '.dat')

dat = t2data_export_json()
dat.title = 'Model intercomparison study problem 2'
dat.simulator = 'AUTOUGH2.2'

dat.grid = t2grid().radial(dr, dz)
rock = dat.grid.rocktypelist[0]
rock.porosity = 0.2
rock.permeability = np.ones(3) * 1.e-14
rock.density = 2650.
rock.conductivity = 0.0
rock.specific_heat = 1000.

dat.relative_permeability = {'type': 1, 'parameters': [0., 0., 0., 0., 0.]}
dat.capillarity = {'type': 1, 'parameters': [0., 0., 1., 0., 0.]}

ndt = 23
dts = 5. * np.logspace(0, ndt - 1, ndt, base = 1.5)
day = 24. * 60. * 60.

P0, T0 = 90.e5, 260.
Tinj = 160.
dat.parameter.update(
    {'max_timesteps': ndt,
     'tstop': 1. * day,
     'print_interval': 1,
     'gravity': None,
     'default_incons': [P0, T0],
     'const_timestep': -int(ceil(ndt / 8.)),
     'timestep': list(dts),
     'relative_error': 1.e-6,
     'absolute_error': 1.
     })


dat.parameter['option'][1] = 1
dat.parameter['option'][11] = 2 # permeability weighting
dat.parameter['option'][12] = 1 # generation table interpolation
dat.parameter['option'][16] = 5
dat.parameter['option'][24] = 2 # initial output

dat.multi = {'eos': 'EW', 'num_components': 1, 'num_phases': 2,
             'num_equations': 2, 'num_secondary_parameters': 6}

lay = geo.layerlist[-1]
col = geo.columnlist[0]
genrate = -14.
blkname = geo.block_name(lay.name, col.name)
gen = t2generator(name = blkname, block = blkname,
                  gx = genrate, type = 'MASS')
dat.add_generator(gen)

dat.write(model_name + '.dat')

# incons:
inc = t2incon()
for blk in dat.grid.blocklist:
    inc[blk.name] = [P0, T0]
inc.write(model_name + '.incon')

dat.run(simulator = 'AUTOUGH2_41Da',
        incon_filename = model_name + '.incon',
        silent = True)

mesh_filename = 'g' + model_name + '.msh'
geo.write_mesh(mesh_filename, dimension = 2, slice = 'x')
jsondata = dat.json(geo, mesh_filename, incons = inc, bdy_incons = inc)
jsondata['initial']['primary'] = [P0, T0]
jsondata['mesh']['radial'] = True
json.dump(jsondata, file(model_name + '.json', 'w'), indent = 2)
                  
os.chdir(orig_dir)
