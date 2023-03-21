import dgl
import numpy as np
import pandas as pd
import warnings

warnings.filterwarnings("ignore")

dataset = dgl.data.CoraGraphDataset('./Data/')
g = dataset[0]

num_class = dataset.num_classes
print('Number of categories:', num_class)

# get node feature
feat = g.ndata['feat']

# get node label
labels = g.ndata['label']

# get edge
net_edges = pd.DataFrame({'from': np.array(g.edges()[0]),
                          'to': np.array(g.edges()[1])})
net_edges['values'] = 1

mat_edges = pd.pivot(net_edges, columns='to', index='from', values='values')
mat_edges = mat_edges.fillna(0)
mat_edges = mat_edges + mat_edges.T
mat_edges = (mat_edges > 0) * 1


# save
np.savetxt('./Data/Network.txt', np.array(mat_edges))
np.savetxt('./Data/Nodes_Labels.txt', labels)
