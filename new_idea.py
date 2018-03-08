import pandas as pd
import random
import numpy as np
import tkinter
import tkinter.messagebox
import tkinter.filedialog
from io import StringIO
import seaborn as sns
import re
import warnings

warnings.filterwarnings("ignore")
sns.set()


# 函数功能
# deal_vissim :        删除fzp文件内的多余文本
# deal_data   :        抽取5%、每隔三秒的数据
# deal_data_with_v :   处理带有速度特征的数据 与deal_data并列使用
# stop_point  :        找到停车特征点以及停车前后的斜率
# add_v       :        增加速度这个参数值
# main()      :        主函数


def deal_vissim(path, save=False, save_path=''):
    # 路径
    # 是否保存
    # 保存路径
    with open(path, 'r') as f:
        data = f.read()
    to_delete = re.findall(r'(\$.*VEHICLE:)', data, re.S)
    if not to_delete == []:
        data = data.replace(to_delete[0], '')

    if save:
        with open(save_path, 'w') as f2:
            f2.write(data)
    else:
        data_io = StringIO(data)
        data = pd.read_csv(data_io, sep=';')
        return data


# test
# df = deal_vissim('F:\\vissim\exp2\\e6.txt',False,'C:\\Users\\D\\Desktop\\1.csv')


def deal_data(df=pd.DataFrame(), lane_path='', save=False, save_path='', pec=0.05, sample=True):
    # 输入dataframe格式数据
    # 输入车道信息数据
    # 输入是否保存
    # 输入保存位置

    # o_lane = int(input('输入起始路段代号:'))
    # d_lane = int(input("输入终止路段代号:"))
    # od_lane = [o_lane]
    # while True:
    #     od = int(input("按顺序输入经过车道代号，输入0结束:"))
    #     if od == 0:
    #         break
    #     else:
    #         od_lane.append(od)
    # od_lane.append(d_lane)
    o_lane = 230
    d_lane = 250
    od_lane = [230, 230240, 240, 240250, 250]

    lane = pd.read_excel(lane_path)
    lane.columns = ['lane_no', 'lane_len', 'lane_num']
    lane_length = []
    for i in od_lane:
        lane_length.append(lane.lane_len.iloc[lane[lane.lane_no == int(i)].index[0]])

    df.columns = ['t', 'vec_no', 'link_no', 'lane_index', 'pos', 'poslat']
    a = df[df.link_no == o_lane].vec_no.unique().tolist()
    b = df[df.link_no == d_lane].vec_no.unique().tolist()
    c = list(set(a).intersection((set(b))))
    df = df[(df.vec_no.isin(c)) & (df.link_no.isin(od_lane))]  # 筛选经过起终点的数据

    df['y'] = None

    def add_len(x):
        index = od_lane.index(x)
        add_num = sum(lane_length[:index])
        return add_num

    df.y = df.pos + df.link_no.apply(add_len)
    # df.to_csv("C:\\Users\\D\\Desktop\\2.csv")

    if sample:
        vec_list = df.vec_no.unique().tolist()
        choice_no = random.sample(vec_list, int(len(vec_list) * pec))
        # print(choice_no)

        result = pd.DataFrame()
        for i in choice_no:
            # print(i)
            result1 = df[df.vec_no == i][df.t % 3 == 0]
            result = result.append(result1)
    else:
        result = df

    if save:
        result.to_csv(save_path)
    else:
        return result


# test
# df = deal_vissim('F:\\vissim\exp2\\e6.txt', False, 'C:\\Users\\D\\Desktop\\1.csv')
# deal_data(df, "C:\\Users\\D\\Desktop\\sc\\lane.xlsx", True, "C:\\Users\\D\\Desktop\\1.csv")
# print(data)


def deal_data_with_v(df=pd.DataFrame(), lane_path='', save=False, save_path='', pec=0.05):
    # 输入dataframe格式数据
    # 输入车道信息数据
    # 输入是否保存
    # 输入保存位置

    # o_lane = int(input('输入起始路段代号:'))
    # d_lane = int(input("输入终止路段代号:"))
    # od_lane = [o_lane]
    # while True:
    #     od = int(input("按顺序输入经过车道代号，输入0结束:"))
    #     if od == 0:
    #         break
    #     else:
    #         od_lane.append(od)
    # od_lane.append(d_lane)
    o_lane = 230
    d_lane = 250
    od_lane = [230, 230240, 240, 240250, 250]

    lane = pd.read_excel(lane_path)
    lane.columns = ['lane_no', 'lane_len', 'lane_num']
    lane_length = []
    for i in od_lane:
        lane_length.append(lane.lane_len.iloc[lane[lane.lane_no == int(i)].index[0]])

    a = df[df.link_no == o_lane].vec_no.unique().tolist()
    b = df[df.link_no == d_lane].vec_no.unique().tolist()
    c = list(set(a).intersection((set(b))))
    df = df[(df.vec_no.isin(c)) & (df.link_no.isin(od_lane))]  # 筛选经过起终点的数据

    df['y'] = None

    def add_len(x):
        index = od_lane.index(x)
        add_num = sum(lane_length[:index])
        return add_num

    df.y = df.pos + df.link_no.apply(add_len)
    # df.to_csv("C:\\Users\\D\\Desktop\\2.csv")

    vec_list = df.vec_no.unique().tolist()
    choice_no = random.sample(vec_list, int(len(vec_list) * pec))
    # print(choice_no)

    result = pd.DataFrame()
    for i in choice_no:
        # print(i)
        result1 = df[df.vec_no == i][df.t % 3 == 0]
        result = result.append(result1)

    if save:
        result.to_csv(save_path)
    else:
        return result


def stop_point(df=pd.DataFrame(), save=False, save_path=None):
    # 输入筛选后的数据
    # 输出特征点数据 vec_no f_t l_t y f_k l_k

    vec_no = df.vec_no.unique().tolist()
    stop = pd.DataFrame(columns=['vec_no', 'f_t', 'l_t', 'y', 'f_k', 'l_k'])
    for vec in vec_no:
        t = np.array(df[df.vec_no == vec].t.tolist())
        Y = np.array(df[df.vec_no == vec].y.tolist())
        k = (Y[1:] - Y[:-1]) / (t[1:] - t[:-1])

        k_max = k.max()
        k_no = np.where(k > k_max / 2)[0]
        k_sep = np.where(k_no[1:] - k_no[:-1] > 1)[0]
        k_split = np.split(k_no, k_sep + 1)
        all_k = []
        for i in k_split:
            all_k.append(np.mean(k[i]))
        all_k = np.array(all_k)
        all_k = all_k.repeat(2)
        all_k = np.delete(all_k, [0, len(all_k) - 1])

        k_min = k.min()
        if k_min > 2:
            continue
        point_no = np.where(k < k_min + 0.5)[0]  # 误差设定为0.5,where 返回的是tuple
        sep_no = np.where(point_no[1:] - point_no[:-1] > 1)[0]
        split_result = np.split(point_no, sep_no + 1)

        if not len(all_k) == 2 * len(split_result):  # 仅提取典型的特征点
            continue
        for i, m in enumerate(split_result):
            i = i * 2
            f_t = t[m[0]]
            l_t = t[m[-1]]
            y = 1 / 2 * (Y[m[0]] + Y[m[-1]])
            stop = stop.append({"vec_no": vec, "f_t": f_t, "l_t": l_t, 'y': y, "f_k": all_k[i], "l_k": all_k[i + 1]},
                               ignore_index=True)

    if save:
        stop.to_csv(save_path)
    else:
        return stop


# test
# df = pd.read_csv("C:\\Users\\D\\Desktop\\1.csv")
# data = deal_vissim('F:\\vissim\exp3\\e2.txt', False, 'C:\\Users\\D\\Desktop\\1.csv')
# df = deal_data(data, "C:\\Users\\D\\Desktop\\sc\\lane.xlsx", False, "C:\\Users\\D\\Desktop\\1.csv")
# print(stop_point(df))


def add_v(df=pd.DataFrame(), save=False, save_path=''):
    # 输入deal_vissim 后的数据 要有y这一列
    # 输出增加y后的数据

    data = pd.DataFrame()
    vec_no = df.vec_no.unique().tolist()
    for vec in vec_no:
        vec_chunk = df[df.vec_no == vec].copy()
        vec_chunk['v'] = None
        t = np.array(vec_chunk.t.tolist())
        y = np.array(vec_chunk.y.tolist())
        v = (y[1:] - y[:-1]) / (t[1:] - t[:-1])
        vec_chunk.v.iloc[:len(vec_chunk) - 1] = v  # iloc 和 loc的区别，注意索引值
        data = data.append(vec_chunk)

    data = data.ffill()
    if save:
        data.to_csv(save_path)
    else:
        return data


# test
# data = deal_vissim('F:\\vissim\exp3\\e2.txt', False, 'C:\\Users\\D\\Desktop\\1.csv')
# df = deal_data(data, "C:\\Users\\D\\Desktop\\sc\\lane.xlsx", False, "C:\\Users\\D\\Desktop\\1.csv", sample=False)
# add_v(df, True, 'C:\\Users\\D\\Desktop\\v.csv')


def queue_length(df=pd.DataFrame(), save=False, save_path=''):
    long = df[df.v == 0].y.mean()
    return long

# test
# df = pd.read_csv("C:\\Users\\D\\Desktop\\v.csv")
# data = deal_data_with_v(df, "C:\\Users\\D\\Desktop\\sc\\lane.xlsx", False, "C:\\Users\\D\\Desktop\\1.csv")
# print(queue_length(data))


def main():
    tkinter.messagebox.showinfo('', '请选择原始文件')
    open_file = tkinter.filedialog.askopenfilename()
    tkinter.messagebox.showinfo('', '请选择车道文件')
    lane_file = tkinter.filedialog.askopenfilename()
    tkinter.messagebox.showinfo('', '请选择保存文件夹')
    save_file = tkinter.filedialog.askdirectory()

    deal = deal_vissim(open_file)
    data = deal_data(deal, lane_file, False, "")
    stop_point(data, True, save_file + '/result.csv')


if __name__ == '__main__':
    main()