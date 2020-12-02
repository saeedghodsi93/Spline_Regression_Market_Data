import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d

x=np.array([0.155, 0.272, 0.414, 0.564, 0.721, 0.855])
y=np.array([0.071, 0.085, 0.134, 0.173, 0.155, 0.198])*100
yn=np.array([0.051, 0.074, 0.123, 0.158, 0.134, 0.164])*100
yp=np.array([0.091, 0.096, 0.145, 0.188, 0.176, 0.232])*100

x_new = np.linspace(x.min(), x.max(),500)
f = interp1d(x, yp, kind='quadratic')
y_smooth=f(x_new)
line_p, = plt.plot (x_new,y_smooth, dashes=[8, 4], label='Upper 95% C.I.')
plt.scatter (x, yp)

x_new = np.linspace(x.min(), x.max(),500)
f = interp1d(x, y, kind='quadratic')
y_smooth=f(x_new)
line_, = plt.plot (x_new,y_smooth, label='mean')
plt.scatter (x, y)

x_new = np.linspace(x.min(), x.max(),500)
f = interp1d(x, yn, kind='quadratic')
y_smooth=f(x_new)
line_n, = plt.plot (x_new,y_smooth, dashes=[8, 4], label='Lower 95% C.I.')
plt.scatter (x, yn)

# plt.legend(handles=[line_, line_n, line_p], )
plt.legend(loc='best')
plt.gca().yaxis.grid(True, linewidth=1)
plt.xlabel('The fourth root of demand share (r4ds)', fontsize=14)
plt.ylabel('Predicted effective discount (%)', fontsize=14)
plt.show()