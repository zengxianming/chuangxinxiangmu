import pandas as pd
import new_find_circle
import numpy as np
import os
import matplotlib.pyplot as plt
from scipy.stats import mode


def find_mode(d_f, vec_no):
    c = d_f[d_f.vec_no == vec_no].len.astype('int').tolist()
    c_mode, mode_count = mode(c)
    first_index = c.index(c_mode) - 1
    last_index = int(mode_count) + int(c.index(c_mode) - 1) + 1
    return first_index, last_index, c_mode


def find_character(df):
    ch = pd.DataFrame(columns=['vec_no', 'f_t', 'l_t', 'y', 'f_k', 'l_k'])
    for vec_no in df.vec_no.unique().tolist():
        y = find_mode(df, vec_no)[2]
        z1 = np.polyfit(df[df.vec_no == vec_no][: find_mode(df, vec_no)[0]].t,
                        df[df.vec_no == vec_no][: find_mode(df, vec_no)[0]].len, 1)
        t1 = (y - z1[1]) / z1[0]
        z2 = np.polyfit(df[df.vec_no == vec_no][find_mode(df, vec_no)[1]:].t,
                        df[df.vec_no == vec_no][find_mode(df, vec_no)[1]:].len, 1)
        t2 = (y - z2[1]) / z2[0]
        ch = ch.append(
            {'vec_no': vec_no, 'f_t': float('%.03f' % t1), 'l_t': float('%.03f' % t2), 'y': float(y), 'f_k': z1[0],
             'l_k': z2[0]}, ignore_index=True)
    return ch


def translation(path):
    dir_name = os.path.split(path)[1]
    circle = new_find_circle.new_find_circle('E:\\character\\' + dir_name)
    df = pd.read_csv(path)
    # plt.scatter(df.t, df.len, s=1, c='r')
    vec = df.vec_no.unique().tolist()
    df1 = pd.DataFrame()
    for vec_no in vec:
        m = df[df.vec_no == vec_no]
        t = np.array(m.t.tolist())
        n = 1
        while True:
            i = np.sum((t - n * circle) < 0) - np.sum((t - (n - 1) * circle) < 0)
            r = np.sum((t - (n+1) * circle) < 0) - np.sum((t - n * circle) < 0)
            if i > 0 and (max(t - (n-1) * circle) < 120):
                break
            n = n + 1

        def move(f):
            return f - (n - 1) * circle

        m.t = m.t.apply(move)
        df1 = df1.append(m, ignore_index=True)
    df1.to_csv('E:\\translation\\' + dir_name)
    df2 = find_character(df1)
    df2.to_csv('E:\\character2\\' + dir_name)
    plt.scatter(df1.t, df1.len, s=0.5)
    plt.scatter(df2.l_t, df2.y, s=1, c='r')
    plt.scatter(df2.f_t, df2.y, s=1, c='yellow')
    plt.savefig('E:\\fig2\\' + dir_name + '.png')
    plt.close()


if __name__ == '__main__':
    dir_list = os.listdir('E:\\result3data')
    for dir_name in dir_list:
        translation('E:\\result3data\\' + dir_name)
